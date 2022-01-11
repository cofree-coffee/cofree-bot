{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import CofreeBot
import CofreeBot.Bot.Calculator.Language
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment.XDG.BaseDir ( getUserCacheDir )
import System.Process.Typed

class CombineBots a b where
  combineBots :: a /\ b -> a

instance {-# OVERLAPPING #-} Monoid a => CombineBots a a where
  combineBots (a :& b) = a <> b

instance (Monoid a, CombineBots a b) => CombineBots a (a /\ b) where
  combineBots (a :& c) = a <> combineBots c


main :: IO ()
main = withProcessWait_ ghciConfig $ \process -> do
  --runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
  --runSimpleBot (ghciBot process) mempty

  --let ghciBot' = simplifySessionBot (T.intercalate "\n") ghciInputParser $ sessionize mempty $ ghciBot process
  --    calcBot = simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
  --runSimpleBot (rmap (\(x :& y) -> x <> y ) $ ghciBot' /\ calcBot) (mempty)

  command <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  let calcBot = liftSimpleBot $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
      helloBot = helloMatrixBot
      coinFlipBot' = liftSimpleBot $ simplifyCoinFlipBot coinFlipBot
      ghciBot' = liftSimpleBot $ ghciBot process
      bot = rmap combineBots $ calcBot /\ helloBot /\ coinFlipBot' /\ ghciBot'
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session xdgCache bot mempty
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session xdgCache bot mempty
