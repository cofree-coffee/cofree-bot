module Main where

import CofreeBot
import Control.Exception
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.IO.Error (isDoesNotExistError)

{-
*This is a very scrappy rough draft*

Initial Goals:

- [x] Synchronous, singlethreaded operation
- [x] Joins a list of rooms provided at startup
- [x] Listens for its name and responds with '@sender did you say my
  name?' in the appropriate channel
- [x] Tracks what messages it has already consumed and uses that to
      avoid sending duplicate responses after restarting
- [ ] Plugin Architecture
- [ ] Administrative inteface (via private message?)
- [ ] Automated Build and Deploy to server
-}

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path =
  (fmap Just $ T.readFile path) `catch` \e ->
    if isDoesNotExistError e
      then pure Nothing
      else throwIO e

main :: IO ()
main = do
  command <- Opt.execParser parserInfo
  since <- readFileMaybe "/tmp/cofree-bot-since_file"
  case command of
    LoginCmd cred -> do
      session <- login cred
      --let cfg = Config session cache respChan
      --connectAndSend cfg
      runListener session since
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runListener session since
