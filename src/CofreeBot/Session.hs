module CofreeBot.Session where

import Control.Concurrent.STM.TVar (modifyTVar, readTVarIO)
import Text.Pretty.Simple
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Network.Matrix.Client
import Network.Matrix.Client.Lens

import CofreeBot.Plugins
import Control.Concurrent.STM.TChan (readTChan)
import Control.Concurrent (forkIO)

pollThread :: ReaderT Config IO ()
pollThread =
  -- NOTE: This needs to get handled properly:
  void $ runExceptT $ do
    Config{..} <- ask
    userId <- ExceptT $ lift $  getTokenOwner session
    filterId <- ExceptT $ lift $ createFilter session userId messageFilter
    _ <- ExceptT $ lift $ getFilter session userId filterId
    syncPoll session (Just filterId) Nothing (Just Online) $ \syncResult -> do
      -- TODO: Having trouble doing the right thing here with
      -- lenses. We want to append to the `tsEvents` list in the
      -- `jrsTimeline` for each entry in the map:
      let rooms' = syncResult ^. _srRooms . _Just . _srrJoin . ifolded
      pPrint rooms'
      liftIO $ atomically $ modifyTVar eventCache (<> rooms')

responseThread :: ReaderT Config IO ()
responseThread = do
  Config{..} <- ask
  forever $ do
    msg <- liftIO $ atomically $ readTChan respChan
    liftIO $ sendResp msg session

dispatchThread :: ReaderT Config IO ()
dispatchThread = forever $ do
  cfg <- ask
  events <- liftIO $ readTVarIO (eventCache cfg)
  let newInvocations = scanEvent <$> fetchNewEvents events
  forM_ newInvocations $ \case
    Nothing -> pure ()
    -- NOTE: I think I need unliftIO here?
    Just lbl -> void $ liftIO $ forkIO $ void $ runReaderT (invokePlugin lbl) cfg

sendResp :: MessageResponse -> ClientSession -> IO ()
sendResp MessageResponse{..} session =
  -- TODO: Error Handling
  void $ sendMessage session room msg txId

-- | Example of sending a message
connectAndSend :: Config -> IO ()
connectAndSend Config{..}= void $ do
  Right userId <- getTokenOwner session
  --print userId
  Right filterId <- createFilter session userId messageFilter
  --print filterId
  Right _ <- getFilter session userId filterId
  --print filter

  sendMessage session (RoomID "!zPTNNaDUkBDMbuceAw:cofree.coffee") (EventRoomMessage (RoomMessageText (MessageText "this was sent by a bot" TextType Nothing Nothing))) (TxnID "1")
