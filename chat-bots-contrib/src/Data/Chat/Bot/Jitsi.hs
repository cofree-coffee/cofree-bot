module Data.Chat.Bot.Jitsi
  ( -- * Bot
    jitsiBot,

    -- * Serializer,
    jitsiSerializer,
  )
where

--------------------------------------------------------------------------------

import Data.Chat.Bot
import Data.Chat.Bot.Jitsi.Dictionary
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.Text (Text)
import Data.Vector qualified as V
import System.Random (randomRIO)

--------------------------------------------------------------------------------

jitsiBot :: Bot IO () () Text
jitsiBot = liftEffect jitsiUrl

--------------------------------------------------------------------------------

jitsiSerializer :: TextSerializer Text ()
jitsiSerializer = S.Serializer {parser, printer = id}

parser :: Text -> Maybe ()
parser i = if (i == "🍐" || i == "pair" || i == "pair") then Just () else Nothing

--------------------------------------------------------------------------------

pickRandomElement :: V.Vector a -> IO a
pickRandomElement vs = do
  i <- randomRIO (0, V.length vs)
  pure $ vs V.! i

jitsiUrl :: IO Text
jitsiUrl = do
  adjective <- pickRandomElement adjectives
  noun <- pickRandomElement pluralNouns
  verb <- pickRandomElement verbs
  adverb <- pickRandomElement adverbs
  let url = "https://meet.jit.si/" <> adjective <> noun <> verb <> adverb
  pure $ url
