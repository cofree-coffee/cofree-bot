{-# LANGUAGE NumDecimals #-}
module CofreeBot.Bot.GHCI where

import CofreeBot.Bot
import Control.Monad
import Control.Monad.Loops (whileM)
import Data.Attoparsec.Text as A
import Data.Text qualified as T
import System.Process.Typed
import System.IO
import GHC.Conc (threadDelay)

type GhciBot = Bot IO () T.Text [T.Text]

hGetOutput :: Handle -> IO String
hGetOutput handle =
  whileM (hReady handle) (hGetChar handle) 
    
ghciBot :: Process Handle Handle () -> GhciBot
ghciBot p = Bot $ \i s -> do
  hPutStrLn (getStdin p) $ T.unpack i 
  hFlush (getStdin p)
  void $ threadDelay 1e5
  o <- hGetOutput (getStdout p)
  pure $ BotAction (pure $ T.pack o) s

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig = setStdin createPipe
          $ setStdout createPipe
          $ shell "ghci 2>&1"
  
ghciInputParser :: Parser T.Text
ghciInputParser = do
  void $ "$ "
  T.pack <$> many1 anyChar 
