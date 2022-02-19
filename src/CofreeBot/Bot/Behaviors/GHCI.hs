{-# LANGUAGE NumDecimals #-}
module CofreeBot.Bot.Behaviors.GHCI
  ( ghciBot
  , ghciConfig
  , hGetOutput
  ) where

import           CofreeBot.Bot
import           CofreeBot.Utils
import           Control.Monad
import           Control.Monad.Loops            ( whileM )
import           Data.Attoparsec.Text          as A
import           Data.Profunctor
import qualified Data.Text                     as T
import           GHC.Conc                       ( threadDelay )
import           System.IO
import           System.Process.Typed
import CofreeBot.MessagingAPI
import Data.String

type GhciBot = Bot IO () T.Text [T.Text]

hGetOutput :: Handle -> IO String
hGetOutput handle = whileM (hReady handle) (hGetChar handle)

ghciBot' :: Process Handle Handle () -> GhciBot
ghciBot' p =
  mapMaybeBot (either (const Nothing) Just . parseOnly ghciInputParser)
    $ Bot
    $ \i s -> do
        hPutStrLn (getStdin p) $ T.unpack i
        hFlush (getStdin p)
        void $ threadDelay 5e5
        o <- hGetOutput (getStdout p)
        pure $ BotAction (pure $ T.pack o) s

ghciBotSafe :: Process Handle Handle () -> GhciBot
ghciBotSafe p =
  dimap (distinguish (/= "ghci: :q")) indistinct
    $  pureStatelessBot (const $ ["I'm Sorry Dave"])
    \/ ghciBot' p

ghciBot :: forall api. (MessagingAPI api, IsString (MessageContent api)) => Process Handle Handle () -> Bot IO () (Channel api, MessageReference api) [APIAction api]
ghciBot p = Bot $ \(chan, msg) s ->
  case runMessageParser ghciInputParser msg of
  Nothing -> pure $ BotAction [] s
  Just i -> fmap (fmap (fmap (APIAction . MkMessage chan . fromString . T.unpack))) $ runBot (ghciBotSafe p) i s

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig = setStdin createPipe $ setStdout createPipe $ shell
  "docker run -i --rm haskell 2>&1"

ghciInputParser :: Parser T.Text
ghciInputParser = do
  void $ "ghci: "
  T.pack <$> many1 anyChar

