module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Text.Pretty.Simple
import Network.Matrix.Client

viewMessage :: RoomEvent -> Maybe T.Text
viewMessage event =
  case reContent $ event of
    (EventRoomMessage (RoomMessageText mt)) ->
      case mt of
        (MessageText txt mtt m_txt ma) -> Just txt
    (EventRoomReply ei rm) -> Nothing
    (EventRoomEdit x1 rm) -> Nothing
    (EventUnknown hm) -> Nothing

connectAndListen :: IO ()
connectAndListen = do
  let token = MatrixToken "xxx"
  sess <- createSession "https://matrix.cofree.coffee" token
  Right userId <- getTokenOwner sess
  --print userId
  Right filterId <- createFilter sess userId messageFilter
  --print filterId
  Right filter <- getFilter sess userId filterId
  --print filter

  void $ syncPoll sess (Just filterId) Nothing (Just Online) $ \syncResult -> do
    let roomData = (fmap . fmap) viewMessage $ M.fromList $ getTimelines syncResult
    pPrint $ roomData M.! RoomID "!zPTNNaDUkBDMbuceAw:cofree.coffee"

connectAndSend :: IO ()
connectAndSend = void $ do
  let token = MatrixToken "xxx"
  sess <- createSession "https://matrix.cofree.coffee" token
  Right userId <- getTokenOwner sess
  --print userId
  Right filterId <- createFilter sess userId messageFilter
  --print filterId
  Right filter <- getFilter sess userId filterId
  --print filter

  sendMessage sess (RoomID "!zPTNNaDUkBDMbuceAw:cofree.coffee") (EventRoomMessage (RoomMessageText (MessageText "this was sent by a bot" TextType Nothing Nothing))) (TxnID "1")

main :: IO ()
main = connectAndListen
