{-# OPTIONS_GHC -Wno-typed-holes #-}
module Main where

import CofreeBot
import CofreeBot.Bot.Calculator.Language
import Control.Exception
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.IO.Error (isDoesNotExistError)

{-
*This is a very scrappy rough draft*

TODO:
- [ ] Bot Lifting Into MatrixBot
  - [x] Prototype
  - [ ] Handle Full RoomEvents
- [ ] Use XDG Directories
- [ ] Bot Tensoring
- [ ] Sessions
- [ ] Automated Build and Deploy to server
- [ ] Test suite
- [ ] Administrative interface (via private message?)
-}

main :: IO ()
main = do
  --runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
  --runSimpleBot (runCalculatorBot $ runSession $ sessionize $ calculatorBot) mempty
  command <- Opt.execParser parserInfo
  let bot = liftSimpleBot $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
  case command of
    LoginCmd cred -> do
      session <- login cred
      --let cfg = Config session cache respChan
      --connectAndSend cfg
      runMatrixBot session bot mempty
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session bot mempty
