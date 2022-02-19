{-# LANGUAGE NumDecimals #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import           CofreeBot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           Control.Monad
import           Data.Profunctor
import           GHC.Conc                       ( threadDelay )
import           Network.Matrix.Client
import qualified Options.Applicative           as Opt
import           OptionsParser
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.Process.Typed
import Data.String
import CofreeBot.MessagingAPI
import GHC.IO.Handle

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

bot :: forall api.
 ( MessagingAPI api
 , IsString (MessageContent api)
 ) => Process Handle Handle () ->
 Bot
   IO
   (CalcState /\ (() /\ (() /\ (() /\ (() /\ (() /\ (() /\ ())))))))
   (Channel api, MessageReference api)
   [APIAction api]
bot process = 
  let foo = sessionize mempty (calculatorBot @api)
     -- calcBot =
     --   liftSimpleBot
     --     $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP
     --     $ sessionize mempty
     --     $ calculatorBot
  in rmap (\(a :& b :& c :& d :& e :& f :& g :& h) -> a <> b <> c <> d <> e <> f <> g <> h)
          $  calculatorBot 
          /\ helloBot
          /\ coinFlipBot
          /\ ghciBot process
          /\ magic8BallBot @()
          /\ updogBot @()
          /\ jitsiBot
          /\ adminBot

cliMain :: IO ()
cliMain = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  runRepl (bot process) mempty

matrixMain :: ClientSession  -> String -> IO ()
matrixMain session xdgCache = withProcessWait_ ghciConfig $ \process -> do
  void $ threadDelay 1e6
  void $ hGetOutput (getStdout process)
  runMatrix session xdgCache (bot process) mempty
