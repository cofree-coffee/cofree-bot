module CofreeBot.Bot.Behaviors.Jitsi where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Jitsi.Dictionary
import           Data.Profunctor
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           System.Random

pickRandomElement :: V.Vector a -> IO a
pickRandomElement vs = do
  i <- randomRIO (0, V.length vs)
  pure $ vs V.! i

jitsiBot' :: Bot IO () () T.Text
jitsiBot' = liftEffect $ do
  adjective <- pickRandomElement adjectives
  noun      <- pickRandomElement pluralNouns
  verb      <- pickRandomElement verbs
  adverb    <- pickRandomElement adverbs
  let url = "https://meet.jit.si/" <> adjective <> noun <> verb <> adverb
  pure $ url

jitsiBot :: TextBot IO ()
jitsiBot =
  dimap
      (\i ->
        if (i == "🍐" || i == "pair" || i == "pair") then Right () else Left ()
      )
      (either id pure)
    $  emptyBot
    \/ jitsiBot'
