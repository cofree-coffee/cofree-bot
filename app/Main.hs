{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import CofreeBot
import CofreeBot.Bot.Behaviors.Calculator.Language
import Control.Monad
import Control.Monad.Except
  ( ExceptT,
    runExceptT,
  )
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (threadDelay)
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Process.Typed

main :: IO ()
main = do
  command <- Opt.execParser parserInfo
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
        liftSimpleBot $
          simplifySessionBot printCalcOutput statementP $
            sessionize mempty $
              calculatorBot
      helloBot = helloMatrixBot
      coinFlipBot' = liftSimpleBot $ simplifyCoinFlipBot coinFlipBot
      ghciBot' = liftSimpleBot $ ghciBot process
      magic8BallBot' = liftSimpleBot $ simplifyMagic8BallBot magic8BallBot
   in calcBot
        /.\ coinFlipBot'
        /.\ helloBot
        /.\ ghciBot'
        /.\ magic8BallBot'
        /.\ updogMatrixBot
        /.\ liftSimpleBot jitsiBot

cliMain :: IO ()
cliMain = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  void $
    loop $
      annihilate repl $
        flip fixBot mempty $
          simplifyMatrixBot $
            bot
              process

unsafeCrashInIO :: Show e => ExceptT e IO a -> IO a
unsafeCrashInIO = runExceptT >=> either (fail . show) pure

matrixMain :: ClientSession -> String -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  unsafeCrashInIO $
    loop $
      annihilate (matrix session xdgCache) $
        batch $
          flip fixBot mempty $
            hoistBot liftIO $
              bot process
