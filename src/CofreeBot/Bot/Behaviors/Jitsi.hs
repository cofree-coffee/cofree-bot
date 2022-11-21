module CofreeBot.Bot.Behaviors.Jitsi
  ( jitsiBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Bot.Behaviors.Jitsi.Dictionary
import CofreeBot.Utils (indistinct)
import Data.Profunctor
import Data.Text qualified as T
import Data.Vector qualified as V
import System.Random

--------------------------------------------------------------------------------

pickRandomElement :: V.Vector a -> IO a
pickRandomElement vs = do
  i <- randomRIO (0, V.length vs)
  pure $ vs V.! i

jitsiBot' :: IO T.Text
jitsiBot' = do
  adjective <- pickRandomElement adjectives
  noun <- pickRandomElement pluralNouns
  verb <- pickRandomElement verbs
  adverb <- pickRandomElement adverbs
  let url = "https://meet.jit.si/" <> adjective <> noun <> verb <> adverb
  pure $ url

jitsiBot :: TextBot IO ()
jitsiBot =
  dimap
    ( \i ->
        if (i == "🍐" || i == "pair" || i == "pair") then Right () else Left ()
    )
    indistinct
    $ emptyBot
      \/ liftEffect jitsiBot'
