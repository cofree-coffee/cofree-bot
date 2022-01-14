{-# LANGUAGE NumDecimals #-}
module Main where

import           CofreeBot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           Data.Profunctor
import qualified Data.Text                     as T
import           Network.Matrix.Client
import qualified Options.Applicative           as Opt
import           OptionsParser
import           System.Environment.XDG.BaseDir ( getUserCacheDir )

--main :: IO ()
--main = withProcessWait_ ghciConfig $ \process -> do
--  runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
--  runSimpleBot (ghciBot process) mempty

--  let ghciBot' = ghciBot process
--      calcBot = simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
--  runSimpleBot (rmap (\(x :& y) -> x <> y ) $ ghciBot' /\ calcBot) (mempty)

main :: IO ()
main = do
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
