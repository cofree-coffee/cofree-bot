module CofreeBot.Bot.Behaviors.Magic8Ball (magic8BallBot) where

import           CofreeBot.Bot
import           CofreeBot.MessagingAPI
import           Data.Attoparsec.Text
import           Data.String
import           System.Random

randomNumber :: Bot IO s () Int
randomNumber = liftEffect $ randomRIO (1, 20)

magic8BallBot :: forall s api. (MessagingAPI api, IsString (MessageContent api)) => Bot IO s (Channel api, MessageReference api) [APIAction api]
magic8BallBot = Bot $ \(chan, msg) s -> case runMessageParser parseCommand msg of
  Nothing -> pure $ BotAction [] s
  Just _ -> fmap (fmap (pure . APIAction . MkMessage chan . mkMsg)) $ runBot randomNumber () s
  where
    mkMsg :: Int -> MessageContent api
    mkMsg i = case i `mod` 20 of
      1  -> "It is certain."
      2  -> "It is decidedly so."
      3  -> "Without a doubt."
      4  -> "Yes definitely."
      5  -> "You may rely on it."
      6  -> "As I see it, yes."
      7  -> "Most likely."
      8  -> "Outlook good."
      9  -> "Yes."
      10 -> "Signs point to yes."
      11 -> "Reply hazy, try again."
      12 -> "Ask again later."
      13 -> "Better not tell you now."
      14 -> "Cannot predict now."
      15 -> "Concentrate and ask again."
      16 -> "Don't count on it."
      17 -> "My reply is no."
      18 -> "My sources say no."
      19 -> "Outlook not so good."
      _  -> "Very doubtful."

parseCommand :: Parser ()
parseCommand = "8 ball" *> pure ()
