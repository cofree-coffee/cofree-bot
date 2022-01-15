{-# LANGUAGE NumDecimals #-}
module Main where

import           CofreeBot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           Control.Monad
import           Data.Profunctor
import qualified Data.Text                     as T
import           GHC.Conc                       ( threadDelay )
import           Network.Matrix.Client
import qualified Options.Applicative           as Opt
import           OptionsParser
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.Process.Typed

main :: IO ()
main = cliMain

cliMain :: IO ()
cliMain = withProcesses replConfigs  $ \Repls{..} -> do
  runTextBot (rmap (\(x :& y :& z :& q :& p) -> x <> y <> z <> q <> p) $
              nodeBot node /\ pythonBot python /\ ghciBot ghci /\ mitSchemeBot mitScheme /\ sbclBot sbcl) mempty 
  --runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
  --let ghciBot' = ghciBot process
  --    calcBot = simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
  --runTextBot (rmap (\(x :& y) -> x <> y ) $ ghciBot' /\ calcBot) (mempty)

matrixMain :: IO ()
matrixMain = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  command  <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  let calcBot =
        liftSimpleBot
          $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP
          $ sessionize mempty
          $ calculatorBot
      helloBot     = helloMatrixBot
      coinFlipBot' = liftSimpleBot $ simplifyCoinFlipBot coinFlipBot
      ghciBot'     = liftSimpleBot $ ghciBot process
      bot =
        rmap (\(x :& y :& z :& q) -> x <> y <> z <> q)
          $  calcBot
          /\ helloBot
          /\ coinFlipBot'
          /\ ghciBot'
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session xdgCache bot mempty
    TokenCmd TokenCredentials {..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session xdgCache bot mempty
