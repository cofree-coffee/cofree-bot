module Data.Chat.Bot.Jitsi
  ( jitsiBot,
  )
where

--------------------------------------------------------------------------------

import Data.Chat.Bot
import Data.Chat.Bot.Jitsi.Dictionary
import Data.Chat.Bot.Monoidal
import Data.Chat.Utils (indistinct)
import Data.Profunctor
import Data.Text (Text)
import Data.Vector qualified as V
import System.Random

--------------------------------------------------------------------------------

pickRandomElement :: V.Vector a -> IO a
pickRandomElement vs = do
  i <- randomRIO (0, V.length vs)
  pure $ vs V.! i

jitsiBot' :: IO Text
jitsiBot' = do
  adjective <- pickRandomElement adjectives
  noun <- pickRandomElement pluralNouns
  verb <- pickRandomElement verbs
  adverb <- pickRandomElement adverbs
  let url = "https://meet.jit.si/" <> adjective <> noun <> verb <> adverb
  pure $ url

jitsiBot :: Bot IO () Text Text
jitsiBot =
  dimap
    ( \i ->
        if (i == "🍐" || i == "pair" || i == "pair") then Right () else Left ()
    )
    indistinct
    $ emptyBot
      \/ liftEffect jitsiBot'
