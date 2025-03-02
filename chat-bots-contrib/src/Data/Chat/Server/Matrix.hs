module Data.Chat.Server.Matrix
  ( -- * Matrix Bot
    MatrixBot,
    matrix,
    embedTextBot,
    RoomID,
    Event,
  )
where

--------------------------------------------------------------------------------

import Control.Lens
import Control.Monad (zipWithM_)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))
import Data.Chat.Bot
import Data.Chat.Bot.Serialization
import Data.Chat.Utils (readFileMaybe)
import Data.Machine.Moore (MooreT (..))
import Data.Map.Strict qualified as Map
import Data.Profunctor
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

-- | A Matrix Server for connecting a 'Bot' to the Matrix protocol.
matrix ::
  forall m.
  (MonadError MatrixError m, MonadIO m) =>
  ClientSession ->
  FilePath ->
  MooreT m [(RoomID, Event)] [(RoomID, Event)]
matrix session cache = MooreT $ do
  -- Setup cache
  liftIO $ createDirectoryIfMissing True cache
  since <- liftIO $ readFileMaybe $ cache <> "/since_file"

  -- Log in
  userId <- liftMatrixIO $ getTokenOwner session
  filterId <- liftMatrixIO $ createFilter session userId messageFilter

  -- Start looping
  runMooreT $ go filterId since
  where
    go :: FilterID -> Maybe Text -> MooreT m [(RoomID, Event)] [(RoomID, Event)]
    go filterId since = MooreT $ do
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
        (events,) $ \outputs -> MooreT $ do
          -- Send the bot's responses
          gen <- newStdGen
          let txnIds = TxnID . Text.pack . show <$> randoms @Int gen
          liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

          -- Update since file
          liftIO $ writeFile (cache <> "/since_file") (Text.unpack newSince)

          -- Do it again
          runMooreT $ go filterId (Just newSince)

embedTextBot :: (Applicative m) => Bot m s Text Text -> Bot m s (RoomID, Event) (RoomID, Event)
embedTextBot = second' . flip applySerializer eventSerializer

eventSerializer :: Serializer Event Event Text Text
eventSerializer =
  Serializer
    { parser = pure . viewBody,
      printer = mkMsg
    }

viewBody :: Event -> Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: Text -> Event
mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
