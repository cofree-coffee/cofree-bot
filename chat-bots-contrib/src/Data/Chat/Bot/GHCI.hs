{-# LANGUAGE NumDecimals #-}

module Data.Chat.Bot.GHCI
  ( -- * Bot
    ghciBot,
    ghciConfig,
    hGetOutput,

    -- * Serializer
    ghciSerializer,
  )
where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops (whileM)
import Data.Attoparsec.Text as A
import Data.Chat.Bot
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Conc (threadDelay)
import System.IO
import System.Process.Typed

--------------------------------------------------------------------------------

ghciBot :: Process Handle Handle () -> Bot IO () Text Text
ghciBot p = Bot $
  \s i -> do
    o <- liftIO $ do
      hPutStrLn (getStdin p) $ Text.unpack i
      hFlush (getStdin p)
      void $ threadDelay 5e5
      hGetOutput (getStdout p)
    pure (Text.pack o, s)

--------------------------------------------------------------------------------

ghciSerializer :: TextSerializer Text Text
ghciSerializer = S.Serializer {parser, printer = id}

parser :: Text -> Maybe Text
parser = either (const Nothing) Just . parseOnly ("ghci:" *> takeText)

--------------------------------------------------------------------------------

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig =
  setStdin createPipe $
    setStdout createPipe $
      shell
        "docker run -i --rm haskell 2>&1"
