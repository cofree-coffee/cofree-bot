{-# LANGUAGE NumDecimals #-}

module Data.Chat.Bot.GHCI
  ( ghciBot,
    ghciConfig,
    hGetOutput,
  )
where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops (whileM)
import Data.Attoparsec.Text as A
import Data.Chat.Bot
import Data.Chat.Bot.Monoidal
import Data.Chat.Utils
import Data.Profunctor
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (threadDelay)
import System.IO
import System.Process.Typed

--------------------------------------------------------------------------------

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

ghciBot' :: Process Handle Handle () -> Bot IO () Text Text
ghciBot' p =
  contramapMaybeBot (either (const Nothing) Just . parseOnly ghciInputParser) $
    Bot $
      \s i -> do
        o <- liftIO $ do
          hPutStrLn (getStdin p) $ Text.unpack i
          hFlush (getStdin p)
          void $ threadDelay 5e5
          hGetOutput (getStdout p)
        pure (Text.pack o, s)

ghciBot :: Process Handle Handle () -> Bot IO () Text Text
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

ghciInputParser :: Parser Text
ghciInputParser = do
  void $ "ghci: "
  Text.pack <$> many1 anyChar
