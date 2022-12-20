{-# LANGUAGE NumDecimals #-}

module CofreeBot.Bot.Behaviors.GHCI
  ( ghciBot,
    ghciConfig,
    hGetOutput,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops (whileM)
import Data.Attoparsec.Text as A
import Data.Profunctor
import Data.Text qualified as T
import GHC.Conc (threadDelay)
import System.IO
import System.Process.Typed

--------------------------------------------------------------------------------

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

ghciBot' :: Process Handle Handle () -> Bot IO () T.Text T.Text
ghciBot' p =
  contraMapMaybeBot (either (const Nothing) Just . parseOnly ghciInputParser) $
    Bot $
      \s i -> do
        o <- liftIO $ do
          hPutStrLn (getStdin p) $ T.unpack i
          hFlush (getStdin p)
          void $ threadDelay 5e5
          hGetOutput (getStdout p)
        pure (T.pack o, s)

ghciBot :: Process Handle Handle () -> Bot IO () T.Text T.Text
ghciBot p =
  dimap (distinguish (/= "ghci: :q")) indistinct $
    pureStatelessBot (const $ "I'm Sorry Dave")
      \/ ghciBot' p

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig =
  setStdin createPipe $
    setStdout createPipe $
      shell
        "docker run -i --rm haskell 2>&1"

ghciInputParser :: Parser T.Text
ghciInputParser = do
  void $ "ghci: "
  T.pack <$> many1 anyChar
