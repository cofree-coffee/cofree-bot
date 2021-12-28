module Main where

import CofreeBot
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad
import Control.Monad.Reader
import Data.Coerce
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser

{-
*This is a very scrappy rough draft*

Initial Goals:

- [x] Synchronous, singlethreaded operation
- [x] Joins a list of rooms provided at startup
- [x] Listens for its name and responds with '@sender did you say my
  name?' in the appropriate channel
- [ ] Tracks what messages it has already consumed and uses that to
      avoid sending duplicate responses after restarting
- [ ] Plugin Architecture
-}

main :: IO ()
main = do
  command <- Opt.execParser parserInfo
  case command of
    LoginCmd cred -> do
      session <- login cred
      --let cfg = Config session cache respChan
      --connectAndSend cfg
      runListener session
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (coerce matrixServer) matrixToken
      runListener session
  
  -- cache <- newTVarIO mempty
  -- respChan <- newTChanIO
  -- void $ forkIO $ runReaderT dispatchThread cfg
  -- void $ forkIO $ runReaderT responseThread cfg
  -- runReaderT pollThread cfg
