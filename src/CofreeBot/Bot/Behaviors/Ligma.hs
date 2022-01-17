{-# LANGUAGE OverloadedStrings #-}

module CofreeBot.Bot.Behaviors.Ligma where

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


ligmaSimpleBot :: Applicative m => Bot m s Text [Text]
ligmaSimpleBot = pureStatelessBot $ runMatches
  [ Match (what <> "ligma") "ligma balls"
  , Match (what <> "updog") "nothin much whats up with you dog"
  , Match (what <> "sugondese") "sugondese nuts"
  ]


ligmaMatrixBot :: Applicative m => MatrixBot m ()
ligmaMatrixBot = liftSimpleBot ligmaSimpleBot

