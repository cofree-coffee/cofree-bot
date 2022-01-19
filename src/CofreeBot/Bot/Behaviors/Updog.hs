module CofreeBot.Bot.Behaviors.Updog where

import           CofreeBot.Bot
import           Control.Applicative (liftA2, empty)
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

newtype Matcher = Matcher
  { runMatcher :: Text -> Bool
  }

instance IsString Matcher where
  fromString t = Matcher (T.isInfixOf $ T.pack t)

instance Semigroup Matcher where
  Matcher p <> Matcher f = Matcher (liftA2 (&&) p f)

instance Monoid Matcher where
  mempty = Matcher $ const True

(|||) :: Matcher -> Matcher -> Matcher
Matcher p ||| Matcher f = Matcher $ liftA2 (||) p f

data Match = Match
  { mMatch :: Matcher
  , mResp :: Text
  }

runMatches :: [Match] -> Text -> [Text]
runMatches ms = flip foldMap ms $ \m t ->
  case runMatcher (mMatch m) t of
    False -> empty
    True -> [ mResp m, "HAH GOTTEM" ]

what :: Matcher
what = "what" ||| "What" ||| "WHAT"

updogSimpleBot :: Monad m => Bot m s Text Text
updogSimpleBot = pureStatelessBot' $ runMatches
  [ Match (what <> "updog") "nothin much whats up with you dog"
  , Match (what <> "snakesay") "Hissss, hisssss"
  , Match (what <> "OPP") "yo, you know me!"
  ]

updogMatrixBot :: Monad m => MatrixBot m ()
updogMatrixBot = liftSimpleBot updogSimpleBot

