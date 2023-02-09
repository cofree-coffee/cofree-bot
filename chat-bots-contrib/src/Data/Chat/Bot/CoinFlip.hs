module Data.Chat.Bot.CoinFlip
  ( -- * Bot
    coinFlipBot,

    -- * Serializer
    coinFlipSerializer,
  )
where

--------------------------------------------------------------------------------

import Data.Attoparsec.Text
import Data.Chat.Bot (Bot)
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random (randomIO)

--------------------------------------------------------------------------------

coinFlipBot :: Bot IO () () Bool
coinFlipBot = randomIO

--------------------------------------------------------------------------------

coinFlipSerializer :: TextSerializer Bool ()
coinFlipSerializer = S.Serializer {parser, printer}

parser :: Text -> Maybe ()
parser = either (const Nothing) Just . parseOnly ("flip a coin" *> pure ())

printer :: Bool -> Text
printer x = "Coin Flip Result: " <> Text.pack (show x)
