module CofreeBot.Session where

import Text.Pretty.Simple
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Maybe (mapMaybe)
import Data.Monoid (Ap(..))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import System.Random (randomIO)

newtype MatrixServer = MatrixServer { getMatrixServer :: T.Text }

findMentions :: RoomEvent -> Maybe RoomEvent
findMentions roomEvent = do
    let formattedMessage = roomEvent ^. _reContent . _EventRoomMessage . _RoomMessageText . _mtBody
    -- href = "<a href=\"https://matrix.to/#/@cofree-bot:cofree.coffee\">cofree-bot</a>"
        name = "cofree-bot"
    if name `T.isInfixOf` formattedMessage
       then Just roomEvent
       else Nothing

respondToMention :: ClientSession -> T.Text -> RoomEvent -> MatrixIO EventID
respondToMention session rid event = do
  let msg = MessageText "Are you talking to me, punk?" TextType Nothing Nothing
  n <- T.pack . show <$> randomIO @Int
  sendMessage session (RoomID rid) (mkReply (RoomID rid) event msg) (TxnID n)

-- NOTE: This needs to get handled properly:
runListener :: ClientSession -> Maybe T.Text -> IO ()
runListener session since =
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
           
           mentionMessages :: Map.Map T.Text [RoomEvent]
           mentionMessages = fmap (mapMaybe findMentions) roomEvents

       liftIO $ writeFile "/tmp/cofree-bot-since_file" (T.unpack newSince)
       pPrint mentionMessages
       getAp $ Map.foldMapWithKey (\rid -> foldMap $ \event -> Ap $ void $ ExceptT $ respondToMention session rid event) mentionMessages

-- | Example of sending a message
connectAndSend :: ClientSession -> IO ()
connectAndSend session = void $ do
  res <- runExceptT $ do
    userId <- ExceptT $ getTokenOwner session
    --print userId
    filterId <- ExceptT $ createFilter session userId messageFilter
    --print filterId
    _ <- liftIO $ getFilter session userId filterId
    --print filter
    pure (userId, filterId)
  case res of
    Left err -> print err
    Right _ ->
      void $ sendMessage session (RoomID "!zPTNNaDUkBDMbuceAw:cofree.coffee") (EventRoomMessage (RoomMessageText (MessageText "this was sent by a bot" TextType Nothing Nothing))) (TxnID "1")
