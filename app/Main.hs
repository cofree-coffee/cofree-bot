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
import Data.Foldable
import GHC.Conc (threadDelay)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP.TLS
import Network.Matrix.Client
import OpenAI.Client (makeOpenAIClient)
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Process.Typed

main :: IO ()
main = do
  command <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  httpManager <- HTTP.newManager HTTP.TLS.tlsManagerSettings
  case command of
    LoginCmd cred openAIKey -> do
      session <- login cred
      matrixMain session xdgCache httpManager openAIKey
    TokenCmd TokenCredentials {..} openAIKey -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      matrixMain session xdgCache httpManager openAIKey
    CLI openAIKey -> cliMain xdgCache httpManager openAIKey

bot process manager (OpenAIKey aiKey) =
  let calcBot =
        embedTextBot $
          simplifySessionBot printCalcOutput statementP $
            sessionize mempty $
              calculatorBot
      helloBot = helloMatrixBot
      coinFlipBot' = embedTextBot $ simplifyCoinFlipBot coinFlipBot
      ghciBot' = embedTextBot $ ghciBot process
      magic8BallBot' = embedTextBot $ simplifyMagic8BallBot magic8BallBot
      openAIBot' = openAIBot $ makeOpenAIClient aiKey manager 2
   in calcBot
        /.\ coinFlipBot'
        /.\ helloBot
        /.\ ghciBot'
        /.\ magic8BallBot'
        /.\ updogMatrixBot
        /.\ embedTextBot jitsiBot
        /.\ embedTextBot openAIBot'

cliMain :: FilePath -> HTTP.Manager -> OpenAIKey -> IO ()
cliMain xdgCache manager openAIKey = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ simplifyMatrixBot $ bot process manager openAIKey
  void $ loop $ annihilate repl fixedBot

unsafeCrashInIO :: Show e => ExceptT e IO a -> IO a
unsafeCrashInIO = runExceptT >=> either (fail . show) pure

matrixMain :: ClientSession -> FilePath -> HTTP.Manager -> OpenAIKey -> IO ()
matrixMain session xdgCache manager openAIKey = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  state <- readState xdgCache
  fixedBot <- flip (fixBotPersistent xdgCache) (fold state) $ hoistBot liftIO $ bot process manager openAIKey
  unsafeCrashInIO $ loop $ annihilate (matrix session xdgCache) $ batch fixedBot
