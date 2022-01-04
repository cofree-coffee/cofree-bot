module Main where

import CofreeBot
import CofreeBot.Bot.Calculator.Language
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client
import Options.Applicative qualified as Opt
import OptionsParser
import System.Environment ( getEnv, lookupEnv )
import System.Environment.XDG.BaseDir ( getUserCacheDir )
{-
*This is a very scrappy rough draft*

TODO:
- [ ] Handle Full RoomEvents
- [ ] Automated Build and Deploy to server
- [ ] Test suite
- [ ] Administrative interface (via private message?)
- [ ] Command to list all sessions
- [ ] Add fixed point of Bot
- [ ] Bot should auto join DMs
-}

findXdgCache :: IO String
findXdgCache = do
  home <- getEnv "HOME"
  fmap (maybe (home <> "/.cache/cofree-bot") (<> "/cofree-bot")) $ lookupEnv "XDG_CACHE_HOME"

main :: IO ()
main = do
  --runSimpleBot (simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot) mempty
  command <- Opt.execParser parserInfo
  xdgCache <- getUserCacheDir "cofree-bot"
  let calcBot = liftSimpleBot $ simplifySessionBot (T.intercalate "\n" . printCalcOutput) programP $ sessionize mempty $ calculatorBot
      helloBot = helloMatrixBot
      bot = rmap (uncurry (<>)) $ calcBot /\ helloBot
  case command of
    LoginCmd cred -> do
      session <- login cred
      runMatrixBot session xdgCache bot mempty
    TokenCmd TokenCredentials{..} -> do
      session <- createSession (getMatrixServer matrixServer) matrixToken
      runMatrixBot session xdgCache bot mempty
