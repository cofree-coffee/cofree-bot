{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.Except
  ( ExceptT,
    runExceptT,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Chat.Bot
import Data.Chat.Bot.Calculator
import Data.Chat.Bot.CoinFlip
import Data.Chat.Bot.Context
import Data.Chat.Bot.GHCI
import Data.Chat.Bot.Hello
import Data.Chat.Bot.Jitsi
import Data.Chat.Bot.Magic8Ball
import Data.Chat.Bot.Monoidal
import Data.Chat.Bot.Serialization qualified as S
import Data.Chat.Bot.Updog
import Data.Chat.Server
import Data.Chat.Server.Matrix
import Data.Chat.Server.Repl
import Data.Foldable (fold)
import GHC.Conc (threadDelay)
import Network.Matrix.Client (ClientSession, login)
import Options
import Options.Applicative qualified as Opt
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Process.Typed (getStdout, withProcessWait_)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

main :: IO ()
main = do
  xdgCache <- getUserCacheDir "cofree-bot"
  Opt.execParser parserInfo >>= \case
    LoginCmd cred -> do
      session <- login cred
      matrixMain session xdgCache
    TokenCmd clientSessionArgv -> do
      putStrLn "Loading Config"
      clientSessionEnv <- fromEnv
      clientSessionConfigFile <- fromConfig
      putStrLn "Starting Cofree-Bot"
      toClientSession (fold [clientSessionArgv, clientSessionEnv, clientSessionConfigFile]) >>= \case
        Just session -> matrixMain session xdgCache
        Nothing -> error "Invalid Client Session"
    CLI -> do
      putStrLn "Starting Cofree-Bot"
      cliMain xdgCache

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

bot' process =
  helloBot @_ @() -- <----- polymorphic states need to get asserted to a monoid
    /+\ updogBot @_ @()
    /+\ coinFlipBot
    /+\ magic8BallBot
    /+\ jitsiBot
    /+\ ghciBot process
    /+\ sessionize mempty calculatorBot

serializer' =
  helloBotSerializer
    S./+\ updogSerializer
    S./+\ coinFlipSerializer
    S./+\ magic8BallSerializer
    S./+\ jitsiSerializer
    S./+\ ghciSerializer
    S./+\ sessionSerializer calculatorSerializer

bot process = S.applySerializer (bot' process) serializer'

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

cliMain :: FilePath -> IO ()
cliMain xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ bot process
  void $ loop $ annihilate repl fixedBot

--------------------------------------------------------------------------------

unsafeCrashInIO :: Show e => ExceptT e IO a -> IO a
unsafeCrashInIO = runExceptT >=> either (fail . show) pure

matrixMain :: ClientSession -> FilePath -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ embedTextBot $ hoistBot liftIO $ bot process
  unsafeCrashInIO $ loop $ annihilate (matrix session xdgCache) $ batch $ fixedBot
