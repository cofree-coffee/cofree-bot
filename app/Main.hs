module Main where

import CofreeBot
import CofreeBot.Bot.Calculator.Language
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment.XDG.BaseDir ( getUserCacheDir )

main :: IO ()
main = do
  --runSimpleBot (simplifyDebugBot (T.intercalate "\n" . printCalcOutput) programP $ debuggize (DebugId "test") $ fmap sequenceA calculatorBot) mempty
  command <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  let calcBot = liftSimpleBot $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
      helloBot = helloMatrixBot
      bot = rmap (uncurry (<>)) $ calcBot /\ helloBot
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session xdgCache bot mempty
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session xdgCache bot mempty
