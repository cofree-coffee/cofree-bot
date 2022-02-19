module CofreeBot.Bot.Behaviors.CoinFlip (coinFlipBot) where

import           CofreeBot.Bot
import           CofreeBot.MessagingAPI
import qualified Data.Attoparsec.Text as A
import           Data.String (IsString)
import           System.Random

coinFlipBot :: forall api. (MessagingAPI api, IsString (MessageContent api)) => Bot IO () (Channel api, MessageReference api) [APIAction api]
coinFlipBot = Bot $ \(chan, msg) s ->
   case runMessageParser parse msg of
     Nothing -> pure $ BotAction [] s
     Just _ -> fmap (fmap (prettyPrint chan)) $ runBot coinFlip () s

coinFlip :: Bot IO () () Bool
coinFlip = liftEffect $ randomIO @Bool

parse :: A.Parser ()
parse = "flip a coin" *> pure ()

prettyPrint :: IsString (MessageContent api) => Channel api -> Bool -> [APIAction api]
prettyPrint chan True = pure $ APIAction $ MkMessage chan $ "Coin Flip Result: True"
prettyPrint chan False = pure $ APIAction $ MkMessage chan $ "Coin Flip Result: False"
