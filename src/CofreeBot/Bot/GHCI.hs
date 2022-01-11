module CofreeBot.Bot.GHCI where


import CofreeBot.Bot
import CofreeBot.Bot.GHCI.Interpreter
import Control.Monad
import Data.Attoparsec.Text as A
import Data.Text qualified as T
import Data.Bifunctor
import Control.Lens

ghciBot :: Bot IO () T.Text [T.Text]
ghciBot = Bot $ \i s -> do
  case parseOnly ghciInputParser i of
    Left _ -> pure $ BotAction [] s
    Right input -> do
      result <- fmap (bimap T.pack (fmap T.pack)) <$> runGhci input
      let result' = result & fmap (\case
           (i', Nothing) -> "```\n> " <> i' <> "\n````"
           (i', Just o) -> "```\n> " <> i' <> "\n" <> o <> "\n```")
      pure $ BotAction result' s

ghciInputParser :: Parser T.Text
ghciInputParser = do
  void $ "$ "
  T.pack <$> many1 anyChar 
