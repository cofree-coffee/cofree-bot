module CofreeBot.Bot.Behaviors.Magic8Ball where

import           CofreeBot.Bot
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( bimap )
import qualified Data.Text                     as T
import           System.Random

magic8BallBot :: Bot IO () () Int
magic8BallBot = Bot $ \_ s -> do
  result <- randomRIO (1, 20)
  pure $ BotAction result s

simplifyMagic8BallBot :: forall s . Bot IO s () Int -> TextBot IO s
simplifyMagic8BallBot (Bot bot) = Bot $ \i s -> case to i of
  Left  _ -> pure $ BotAction [] s
  Right _ -> fmap (fmap from) $ bot () s
 where
  to :: T.Text -> Either T.Text ()
  to = fmap (bimap T.pack id) $ parseOnly parseMagic8BallCommand

  from :: Int -> [T.Text]
  from i = case i `mod` 20 of
    1  -> pure "It is certain."
    2  -> pure "It is decidedly so."
    3  -> pure "Without a doubt."
    4  -> pure "Yes definitely."
    5  -> pure "You may rely on it."
    6  -> pure "As I see it, yes."
    7  -> pure "Most likely."
    8  -> pure "Outlook good."
    9  -> pure "Yes."
    10 -> pure "Signs point to yes."
    11 -> pure "Reply hazy, try again."
    12 -> pure "Ask again later."
    13 -> pure "Better not tell you now."
    14 -> pure "Cannot predict now."
    15 -> pure "Concentrate and ask again."
    16 -> pure "Don't count on it."
    17 -> pure "My reply is no."
    18 -> pure "My sources say no."
    19 -> pure "Outlook not so good."
    _  -> pure "Very doubtful."

parseMagic8BallCommand :: Parser ()
parseMagic8BallCommand = "8 ball" *> pure ()
