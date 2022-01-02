{-# OPTIONS_GHC -Wno-typed-holes #-}
module Main where

import CofreeBot
import CofreeBot.Bot.Calculator.Language
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser

{-
*This is a very scrappy rough draft*

TODO:
- [ ] Handle Full RoomEvents
- [ ] Use XDG Directories
- [ ] Automated Build and Deploy to server
- [ ] Test suite
- [ ] Administrative interface (via private message?)
- [ ] Command to list all sessions
- [ ] Add fixed point of Bot
-}

main :: IO ()
main = do
  --runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
  command <- Opt.execParser parserInfo
  let calcBot = liftSimpleBot $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
      helloBot = helloMatrixBot
      bot = rmap (uncurry (<>)) $ calcBot /\ helloBot
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session bot mempty
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session bot mempty
