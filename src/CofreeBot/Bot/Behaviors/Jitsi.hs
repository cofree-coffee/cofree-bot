module CofreeBot.Bot.Behaviors.Jitsi where

import CofreeBot.Bot
import CofreeBot.Bot.Behaviors.Jitsi.Dictionary
import qualified Data.Text as T
import Data.Profunctor
import System.Random
import qualified Data.Vector as V

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

jitsiBot :: TextBot IO ()
jitsiBot =
  dimap (\i -> if (i == "🍐" || i == "pair" || i == "pair") then Right () else Left ()) (either id pure) $ emptyBot \/ jitsiBot'
