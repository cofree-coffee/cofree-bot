-- | Matrix Server
module CofreeBot.Server.Matrix where

import           CofreeBot.Bot                  ( Bot(..)
                                                , BotAction(..)
                                                )
import           CofreeBot.Server.Type
import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Network.Matrix.Client
import           Network.Matrix.Client.Lens
import           System.Directory
import           System.IO.Error
import           System.Random

type MatrixBot m s = Bot m s (RoomID, Event) [(RoomID, Event)]

liftMatrixIO :: (MonadIO m, MonadError MatrixError m) => MatrixIO x -> m x
liftMatrixIO m = liftEither =<< liftIO m

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path = fmap Just (T.readFile path)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

matrix
  :: (MonadError MatrixError m, MonadIO m)
  => ClientSession
  -> FilePath
  -> m (Server m [(RoomID, Event)] [(RoomID, Event)])
matrix session cache = do
  -- Setup cache
  liftIO $ createDirectoryIfMissing True cache
  since    <- liftIO $ readFileMaybe $ cache <> "/since_file"

  -- Log in
  userId   <- liftMatrixIO $ getTokenOwner session
  filterId <- liftMatrixIO $ createFilter session userId messageFilter

  -- Start looping
  go filterId since

 where
  go filterId since = do
    -- Get conversation events
    syncResult <- liftMatrixIO
      $ sync session (Just filterId) since (Just Online) (Just 1000)

    -- Unpack sync result
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

    return $ Server $ (events, ) $ \outputs -> do
      -- Send the bot's responses
      gen <- newStdGen
      let txnIds = TxnID . T.pack . show <$> randoms @Int gen
      liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

      -- Update since file
      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)

      -- Do it again
      go filterId (Just newSince)

simplifyMatrixBot :: Monad m => MatrixBot m s -> Bot m s T.Text [T.Text]
simplifyMatrixBot (Bot bot) = Bot $ \i s -> do
  BotAction {..} <- bot (RoomID mempty, mkMsg i) s
  pure $ BotAction (fmap (viewBody . snd) responses) nextState

liftSimpleBot :: Functor m => Bot m s T.Text [T.Text] -> MatrixBot m s
liftSimpleBot (Bot bot) =
  Bot $ \(rid, i) s -> fmap (fmap (fmap ((rid, ) . mkMsg))) $ bot (viewBody i) s

viewBody :: Event -> T.Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: T.Text -> Event
mkMsg msg =
  EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
