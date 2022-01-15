{-# LANGUAGE NumDecimals #-}
module Main where

import           CofreeBot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           Control.Monad
import           Data.Profunctor
import qualified Data.Text                     as T
import           GHC.Conc (threadDelay)
import           Network.Matrix.Client
import qualified Options.Applicative           as Opt
import           OptionsParser
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.Process.Typed
import Data.Foldable (traverse_)

main :: IO ()
main = cliMain

cliMain :: IO ()
cliMain = withProcesses replConfigs  $ \handles@Repls{..} -> do
  void $ threadDelay 1e7
  traverse_ (hGetOutput . getStdout) handles
  runTextBot (rmap (\(x :& y :& z :& q) -> x <> y <> z <> q) $
              nodeBot node /\ pythonBot python /\ ghciBot ghci /\ mitSchemeBot mitScheme) mempty 
  --runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
  --let ghciBot' = ghciBot process
  --    calcBot = simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
  --runTextBot (rmap (\(x :& y) -> x <> y ) $ ghciBot' /\ calcBot) (mempty)

matrixMain :: IO ()
matrixMain = withProcesses replConfigs $ \Repls{..} -> do
  void $ threadDelay 1e7
  void $ hGetOutput (getStdout ghci)
  command  <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  let calcBot =
        liftSimpleBot
          $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP
          $ sessionize mempty
          $ calculatorBot
      helloBot     = helloMatrixBot
      coinFlipBot' = liftSimpleBot $ simplifyCoinFlipBot coinFlipBot
      bot =
        rmap (\(x :& y :& z) -> x <> y <> z)
          $  calcBot
          /\ helloBot
          /\ coinFlipBot'
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session xdgCache bot mempty
    TokenCmd TokenCredentials {..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session xdgCache bot mempty
