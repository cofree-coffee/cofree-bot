module CofreeBot.Bot.Matrix where

import Control.Lens qualified as L
import CofreeBot.Bot ( BotAction(..), Bot(..) )
import Control.Lens
import Control.Monad.Except
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import Text.Pretty.Simple
import Data.IORef
import System.Random ( newStdGen, randoms )
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, throwIO)

-- | A 'MatrixBot' maps from 'RoomEvent' to '[RoomEvent]'
type MatrixBot m s = Bot m s (RoomID, Event) (RoomID, [Event])

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path =
  (fmap Just $ T.readFile path) `catch` \e ->
    if isDoesNotExistError e
      then pure Nothing
      else throwIO e

runMatrixBot :: forall s. ClientSession -> MatrixBot IO s -> s -> IO ()
runMatrixBot session bot s = do
  ref <- newIORef s
  since <- readFileMaybe "/tmp/cofree-bot-since_file"
  void $ runExceptT $ do
    userId <- ExceptT $ getTokenOwner session
    filterId <- ExceptT $ createFilter session userId messageFilter
    syncPoll session (Just filterId) since (Just Online) $ \syncResult -> do
       let newSince :: T.Text
           newSince = syncResult ^. _srNextBatch

           roomsMap :: Map.Map T.Text JoinedRoomSync
           roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded 

           roomEvents :: Map.Map T.Text [RoomEvent]
           roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)
           
           events :: [(RoomID, Event)]
           events = Map.foldMapWithKey (\rid es -> fmap ((RoomID rid,) . view _reContent) es) roomEvents

       liftIO $ writeFile "/tmp/cofree-bot-since_file" (T.unpack newSince)
       pPrint roomEvents
       traverse_ (go ref) events
  where
    go :: MonadIO m => IORef s -> (RoomID, Event) -> m ()
    go ref input = do
      state <- liftIO $ readIORef ref
      BotAction {..} <- liftIO $ runBot bot input state
      liftIO $ writeIORef ref nextState
      gen <- newStdGen
      let txnIds = (TxnID . T.pack . show <$> randoms @Int gen)
      liftIO $ traverse_ (uncurry $ sendMessage session (fst input)) $ zip (snd responses) txnIds

liftMaybeTextToEvent :: Functor f => Bot f s T.Text (Maybe T.Text) -> Bot f s Event [Event]
liftMaybeTextToEvent (Bot bot) = Bot $ \i s -> fmap from $ bot (to i) s
  where
    to :: Event -> T.Text
    to = L.view (_EventRoomMessage . _RoomMessageText . _mtBody)

    from :: BotAction s (Maybe T.Text) -> BotAction s [Event]
    from x = fmap (fmap mkMsg . toList) x

    mkMsg :: T.Text -> Event
    mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
