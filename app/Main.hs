{-# LANGUAGE NumDecimals #-}

module Main where

--------------------------------------------------------------------------------

import CofreeBot
import CofreeBot.Bot.Behaviors.Calculator.Language (statementP)
import Control.Monad (void, (>=>))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (fold)
import GHC.Conc (threadDelay)
import Network.Matrix.Client (ClientSession, login)
import Options
import Options.Applicative qualified as Opt
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Process.Typed (getStdout, withProcessWait_)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  xdgCache <- getUserCacheDir "cofree-bot"
  Opt.execParser parserInfo >>= \case
    LoginCmd cred -> do
      session <- login cred
      matrixMain session xdgCache
    TokenCmd clientSessionArgv -> do
      clientSessionEnv <- fromEnv
      clientSessionConfigFile <- fromConfig
      toClientSession (fold [clientSessionArgv, clientSessionEnv, clientSessionConfigFile]) >>= \case
        Just session -> matrixMain session xdgCache
        Nothing -> error "Invaid Client Session"
    CLI -> cliMain xdgCache

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

cliMain :: FilePath -> IO ()
cliMain xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ simplifyMatrixBot $ bot process
  void $ loop $ annihilate repl fixedBot

--------------------------------------------------------------------------------

unsafeCrashInIO :: Show e => ExceptT e IO a -> IO a
unsafeCrashInIO = runExceptT >=> either (fail . show) pure

matrixMain :: ClientSession -> FilePath -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ hoistBot liftIO $ bot process
  unsafeCrashInIO $ loop $ annihilate (matrix session xdgCache) $ batch fixedBot
