module CofreeBot.Bot.Behaviors.Jitsi (jitsiBot) where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Jitsi.Dictionary
import           CofreeBot.MessagingAPI
import           Control.Applicative
import qualified Data.Attoparsec.Text as A
import           Data.String
import qualified Data.Text as T
import qualified Data.Vector as V
import           System.Random

pickRandomElement :: V.Vector a -> IO a
pickRandomElement vs = do
  i <- randomRIO (0, V.length vs)
  pure $ vs V.! i

jitsiBot' :: Bot IO () () T.Text
jitsiBot' = Bot $ \_ s -> do
  adjective <- pickRandomElement adjectives
  noun <- pickRandomElement pluralNouns
  verb <- pickRandomElement verbs
  adverb <- pickRandomElement adverbs

  let url = "https://meet.jit.si/" <> adjective <> noun <> verb <> adverb
  pure $ BotAction url s

jitsiBot :: forall api. (MessagingAPI api, IsString (MessageContent api)) => Bot IO () (Channel api, MessageReference api) [APIAction api]
jitsiBot = Bot $ \(chan, msg) s ->
  case runMessageParser parseCommand msg of
    Nothing -> pure $ BotAction [] s
    Just _ -> fmap (fmap (pure . APIAction . MkMessage chan . fromString . T.unpack)) $ runBot jitsiBot' () s
  --dimap (\i -> if (i == "🍐" || i == "pair" || i == "pear") then Right () else Left ()) (either id pure) $ emptyBot \/ jitsiBot'


parseCommand :: A.Parser ()
parseCommand = () <$ ("🍐" <|> "pair" <|> "pear")

