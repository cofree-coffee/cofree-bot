module Data.Chat.Bot.Updog
  ( -- * Bot
    updogBot,

    -- * Serializer
    Updog (..),
    updogBotParser,
    updogSerializer,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (liftA2)
import Control.Monad.ListT (toListT)
import Data.Chat.Bot
import Data.Chat.Bot.Serialization
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

updogBot :: (Monad m) => Bot m s Updog Text
updogBot = Bot $ \s -> \case
  Updog -> toListT [("nothin much whats up with you dog", s), ("HAH GOTTEM", s)]
  Snakesay -> toListT [("Hissss, hisssss", s), ("HAH GOTTEM", s)]
  OPP -> toListT [("yo, you know me!", s), ("HAH GOTTEM", s)]

--------------------------------------------------------------------------------

data Updog = Updog | Snakesay | OPP
  deriving (Show, Read)

updogBotParser :: Text -> Maybe Updog
updogBotParser msg
  | runMatcher (what <> "updog") msg = Just Updog
  | runMatcher (what <> "snakesay") msg = Just Snakesay
  | runMatcher (what <> "OPP") msg = Just OPP
  | otherwise = Nothing

updogSerializer :: TextSerializer Text Updog
updogSerializer = Serializer updogBotParser id

--------------------------------------------------------------------------------

newtype Matcher = Matcher
  { runMatcher :: Text -> Bool
  }

instance IsString Matcher where
  fromString = Matcher . Text.isInfixOf . Text.pack

instance Semigroup Matcher where
  Matcher p <> Matcher f = Matcher (liftA2 (&&) p f)

instance Monoid Matcher where
  mempty = Matcher $ const True

(|||) :: Matcher -> Matcher -> Matcher
Matcher p ||| Matcher f = Matcher $ liftA2 (||) p f

what :: Matcher
what = "what" ||| "What" ||| "WHAT"
