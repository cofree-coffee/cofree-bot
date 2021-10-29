module CofreeBot where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.Coerce
import qualified Data.Text as T
import Text.Pretty.Simple
import Network.Matrix.Client
import Network.Matrix.Client.Lens

data Config = Config
  { token :: MatrixToken
  , server :: MatrixServer
  }

newtype MatrixServer = MatrixServer T.Text

viewMessage :: RoomEvent -> Maybe T.Text
viewMessage event =
  case reContent event of
    (EventRoomMessage (RoomMessageText mt)) ->
      case mt of
        (MessageText txt _ _ _) -> Just txt
    (EventRoomReply _ _) -> Nothing
    (EventRoomEdit _ _) -> Nothing
    (EventUnknown _) -> Nothing

connectAndListen :: Config -> IO ()
connectAndListen Config{..} = do
  sess <- createSession (coerce server) token
  void $ runExceptT $ do
    userId <- ExceptT $ getTokenOwner sess
    filterId <- ExceptT $ createFilter sess userId messageFilter
    _ <- ExceptT $ getFilter sess userId filterId
    syncPoll sess (Just filterId) Nothing (Just Online) $ \syncResult -> do
      let timeline = syncResult ^..
                                _srRooms
                              . _Just
                              . _srrJoin
                              . _Just
                              . ix "!zPTNNaDUkBDMbuceAw:cofree.coffee"
                              . _jrsTimeline
                              . _tsEvents
                              . _Just
                              . folded
                              . filteredBy (_reType . only "m.room.message")
      pPrint timeline


connectAndSend :: IO ()
connectAndSend = void $ do
  let token = MatrixToken "xxx"
  sess <- createSession "https://matrix.cofree.coffee" token
  Right userId <- getTokenOwner sess
  --print userId
  Right filterId <- createFilter sess userId messageFilter
  --print filterId
  Right _ <- getFilter sess userId filterId
  --print filter

  sendMessage sess (RoomID "!zPTNNaDUkBDMbuceAw:cofree.coffee") (EventRoomMessage (RoomMessageText (MessageText "this was sent by a bot" TextType Nothing Nothing))) (TxnID "1")
