module CofreeBot.Bot.Behaviors.Updog where

import           CofreeBot.Bot ( Bot (..), BotAction (BotAction) )
import           CofreeBot.Bot.Harness.Repl ()
import           CofreeBot.MessagingAPI
import           Control.Applicative (liftA2, empty)
import qualified Data.Attoparsec.Text as A
import           Data.String ( IsString(..) )
import qualified Data.Text as T

newtype Matcher = Matcher
  { runMatcher :: T.Text -> Bool
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
  , mResp :: T.Text
  }

runMatches :: [Match] -> T.Text -> [T.Text]
runMatches ms = flip foldMap ms $ \m t ->
  case runMatcher (mMatch m) t of
    False -> empty
    True -> [ mResp m, "HAH GOTTEM" ]

what :: Matcher
what = "what" ||| "What" ||| "WHAT"

matcher :: T.Text -> [T.Text]
matcher = runMatches
  [ Match (what <> "updog") "nothin much whats up with you dog"
  , Match (what <> "snakesay") "Hissss, hisssss"
  , Match (what <> "OPP") "yo, you know me!"
  ]

updogBot :: forall s api. (MessagingAPI api, IsString (MessageContent api)) => Bot IO s (Channel api, MessageReference api) [APIAction api]
updogBot = Bot $ \(chan, msg) s ->
  case runMessageParser parseText msg of
    Nothing -> pure $ BotAction [] s
    Just i ->
      let response = fmap (APIAction . MkMessage chan . fromString . T.unpack) $ matcher i
       in pure $ BotAction response s 

parseText :: A.Parser T.Text
parseText = fmap T.pack $ A.many1 A.anyChar
