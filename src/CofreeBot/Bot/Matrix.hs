module CofreeBot.Bot.Matrix where

import           CofreeBot.Bot                  ( Bot(..)
                                                , BotAction(..)
                                                )
import           CofreeBot.Bot.Simple
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Control.Monad.Except
import           Data.Foldable
import           Data.IORef
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Network.Matrix.Client
import           Network.Matrix.Client.Lens
import           System.Directory               ( createDirectoryIfMissing )
import           System.IO.Error                ( isDoesNotExistError )
import           System.Random                  ( newStdGen
                                                , randoms
                                                )

type MatrixBot m s = Bot m s (RoomID, Event) [(RoomID, Event)]

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path = (fmap Just $ T.readFile path)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

runMatrixBot
  :: forall s . ClientSession -> String -> MatrixBot IO s -> s -> IO ()
runMatrixBot session cache bot s = do
  ref <- newIORef s
  createDirectoryIfMissing True cache
  since <- readFileMaybe $ cache <> "/since_file"
  void $ runExceptT $ do
    userId   <- ExceptT $ getTokenOwner session
    filterId <- ExceptT $ createFilter session userId messageFilter
    syncPoll session (Just filterId) since (Just Online) $ \syncResult -> do
      let newSince :: T.Text
          newSince = syncResult ^. _srNextBatch

          roomsMap :: Map.Map T.Text JoinedRoomSync
          roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded

          roomEvents :: Map.Map T.Text [RoomEvent]
          roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

          events :: [(RoomID, Event)]
          events = Map.foldMapWithKey
            (\rid es -> fmap ((RoomID rid, ) . view _reContent) es)
            roomEvents

      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)
      --print roomEvents
      traverse_ (go ref) events
 where
  go :: MonadIO m => IORef s -> (RoomID, Event) -> m ()
  go ref input = do
    state          <- liftIO $ readIORef ref
    BotAction {..} <- liftIO $ runBot bot input state
    liftIO $ writeIORef ref nextState
    gen <- newStdGen
    let txnIds = (TxnID . T.pack . show <$> _ gen)
    liftIO $ sequence_ $ zipWith (uncurry $ sendMessage session)
                                 responses
                                 txnIds

liftSimpleBot :: Functor m => TextBot m s -> MatrixBot m s
liftSimpleBot (Bot bot) = Bot
  $ \(rid, i) s -> fmap (fmap (fmap ((rid, ) . mkMsg))) $ bot (to i) s
 where
  viewBody :: Event -> T.Text
  viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

  to :: Event -> T.Text
  to = viewBody

  mkMsg :: T.Text -> Event
  mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg
                                                               TextType
                                                               Nothing
                                                               Nothing
