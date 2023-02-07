module Data.Chat.Server.Matrix
  ( -- * Matrix Bot
    MatrixBot,
    matrix,
    simplifyMatrixBot,
    embedTextBot,
    RoomID,
    Event,
  )
where

--------------------------------------------------------------------------------

import Control.Lens
import Control.Monad.Except
import Data.Chat.Bot
import Data.Chat.Server
import Data.Chat.Utils (readFileMaybe)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import System.Directory (createDirectoryIfMissing)
import System.Random

--------------------------------------------------------------------------------

type MatrixBot m s = Bot m s (RoomID, Event) (RoomID, Event)

liftMatrixIO :: (MonadIO m, MonadError MatrixError m) => MatrixIO x -> m x
liftMatrixIO m = liftEither =<< liftIO m

-- | A Matrix 'Server' for connecting a 'Bot' to the Matrix protocol.
matrix ::
  forall m.
  (MonadError MatrixError m, MonadIO m) =>
  ClientSession ->
  FilePath ->
  Server m (RoomID, Event) [(RoomID, Event)]
matrix session cache = Server $ do
  -- Setup cache
  liftIO $ createDirectoryIfMissing True cache
  since <- liftIO $ readFileMaybe $ cache <> "/since_file"

  -- Log in
  userId <- liftMatrixIO $ getTokenOwner session
  filterId <- liftMatrixIO $ createFilter session userId messageFilter

  -- Start looping
  runServer $ go filterId since
  where
    go :: FilterID -> Maybe Text -> Server m (RoomID, Event) [(RoomID, Event)]
    go filterId since = Server $ do
      -- Get conversation events
      syncResult <-
        liftMatrixIO $
          sync session (Just filterId) since (Just Online) (Just 1000)

      -- Unpack sync result
      let newSince :: Text
          newSince = syncResult ^. _srNextBatch

          roomsMap :: Map.Map Text JoinedRoomSync
          roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded

          roomEvents :: Map.Map Text [RoomEvent]
          roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

          events :: [(RoomID, Event)]
          events =
            Map.foldMapWithKey
              (\rid es -> fmap ((RoomID rid,) . view _reContent) es)
              roomEvents

      pure $
        (events,) $ \outputs -> Server $ do
          -- Send the bot's responses
          gen <- newStdGen
          let txnIds = TxnID . Text.pack . show <$> randoms @Int gen
          liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

          -- Update since file
          liftIO $ writeFile (cache <> "/since_file") (Text.unpack newSince)

          -- Do it again
          runServer $ go filterId (Just newSince)

-- | Map the input and output of a 'MatrixBot' to allow for simple
-- 'Text' I/O.
simplifyMatrixBot :: Monad m => MatrixBot m s -> Bot m s Text Text
simplifyMatrixBot (Bot bot) = Bot $ \s i -> do
  (responses, nextState) <- bot s (RoomID mempty, mkMsg i)
  pure (viewBody $ snd responses, nextState)

embedTextBot :: Functor m => Bot m s Text Text -> MatrixBot m s
embedTextBot (Bot bot) = Bot $ \s (rid, i) ->
  fmap (\(i', s') -> ((rid, mkMsg i'), s')) $ bot s (viewBody i)

viewBody :: Event -> Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: Text -> Event
mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
