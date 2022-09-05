{-# LANGUAGE NumDecimals #-}
module CofreeBot.Bot.Behaviors.GHCI
  ( ghciBot
  , ghciConfig
  , hGetOutput
  ) where

import           CofreeBot.Bot
import           CofreeBot.Utils
import           CofreeBot.Utils.ListT
import           Control.Monad
import           Control.Monad.Loops            ( whileM )
import           Data.Attoparsec.Text          as A
import           Data.Profunctor
import qualified Data.Text                     as T
import           GHC.Conc                       ( threadDelay )
import           System.IO
import           System.Process.Typed

type GhciBot = Bot IO () T.Text T.Text

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

ghciBot' :: Process Handle Handle () -> GhciBot
ghciBot' p =
  mapMaybeBot (either (const Nothing) Just . parseOnly ghciInputParser)
    $ Bot
    $ \s i -> ListT $ do
        hPutStrLn (getStdin p) $ T.unpack i
        hFlush (getStdin p)
        void $ threadDelay 5e5
        o <- hGetOutput (getStdout p)
        pure (ConsF ((T.pack o), s) emptyListT)

ghciBot :: Process Handle Handle () -> GhciBot
ghciBot p =
  dimap (distinguish (/= "ghci: :q")) indistinct
    $  pureStatelessBot (const $ "I'm Sorry Dave")
    \/ ghciBot' p

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig = setStdin createPipe $ setStdout createPipe $ shell
  "docker run -i --rm haskell 2>&1"

ghciInputParser :: Parser T.Text
ghciInputParser = do
  void $ "ghci: "
  T.pack <$> many1 anyChar

