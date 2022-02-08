{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
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
main = do
  command  <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"

  case command of
    LoginCmd cred -> do
      session <- login cred
      matrixMain session xdgCache
    TokenCmd TokenCredentials {..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      matrixMain session xdgCache
    CLI -> cliMain

bot process =
  let calcBot =
        liftSimpleBot
          $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP
          $ sessionize mempty
          $ calculatorBot
      helloBot     = helloMatrixBot
      coinFlipBot' = liftSimpleBot $ simplifyCoinFlipBot coinFlipBot
      ghciBot'     = liftSimpleBot $ ghciBot process
      magic8BallBot' = liftSimpleBot $ simplifyMagic8BallBot magic8BallBot
  in rmap (\(x :& y :& z :& q :& w :& p :& r) -> x <> y <> z <> q <> w <> p <> r)
          $  calcBot
          /\ helloBot
          /\ coinFlipBot'
          /\ ghciBot'
          /\ magic8BallBot'
          /\ updogMatrixBot
          /\ liftSimpleBot jitsiBot

cliMain :: IO ()
cliMain = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  runTextBot (simplifyMatrixBot $ bot process) mempty

matrixMain :: ClientSession  -> String -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  runMatrixBot session xdgCache (bot process) mempty
