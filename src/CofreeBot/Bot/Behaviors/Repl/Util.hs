{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module CofreeBot.Bot.Behaviors.Repl.Util where

import           CofreeBot.Bot
import           Control.Exception (bracket)
import           Control.Monad
import           Control.Monad.Loops            ( whileM )
import           Data.Attoparsec.Text          as A
import           Data.Foldable
import qualified Data.Text                     as T
import           GHC.Conc                       ( threadDelay )
import           System.IO
import           System.Process.Typed

type ReplBot = Bot IO () T.Text [T.Text]

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

replBot :: T.Text -> Process Handle Handle () -> ReplBot
replBot prompt p =
  mapMaybeBot (either (const Nothing) Just . parseOnly (replInputParser prompt))
    $ Bot
    $ \i s -> do
        hPutStrLn (getStdin p) $ T.unpack i
        hFlush (getStdin p)
        void $ threadDelay 1e6
        o <- hGetOutput (getStdout p)
        pure $ BotAction (pure $ T.pack o) s

replConfig :: String -> ProcessConfig Handle Handle ()
replConfig = setStdin createPipe . setStdout createPipe . shell 

replInputParser :: T.Text -> Parser T.Text
replInputParser prompt = do
  void $ string prompt
  T.pack <$> many1 anyChar

data Repls a = Repls
  { python :: a
  , ghci :: a
  , node :: a
  , mitScheme :: a
  } deriving (Functor, Foldable, Traversable)

withProcesses :: Repls (ProcessConfig i o e) -> (Repls (Process i o e) -> IO r) -> IO r
withProcesses cfgs = bracket (traverse startProcess cfgs) (traverse_ stopProcess)
