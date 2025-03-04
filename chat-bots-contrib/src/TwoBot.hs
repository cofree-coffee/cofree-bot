{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TwoBot where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Chat.Bot
import Data.Chat.Bot.Calculator (CalcError, CalcResp, CalcState, Statement, calculatorBot, calculatorSerializer)
import Data.Chat.Bot.Context (SessionInput, SessionOutput, SessionState, sessionSerializer, sessionize)
import Data.Chat.Bot.HKD
import Data.Chat.Bot.Hello
import Data.Chat.Bot.Serialization (Serializer (..), TextSerializer, applySerializer)
import Data.Text (Text)
import GHC.Generics (Generic, Generically (..))

--------------------------------------------------------------------------------

newtype Flip b l r = Flip {runFlip :: b r l}
  deriving (Eq, Ord, Read, Show)

newtype PhantomState p s i o = PhantomState {unPhantomState :: (p i o)}

--------------------------------------------------------------------------------

data HKDTwo p = HKDTwo
  { hello :: p () () Text,
    calculator :: p (SessionState CalcState) (SessionInput Statement) (SessionOutput (Either CalcError CalcResp))
  }
  deriving stock (Generic)
  deriving anyclass (SequenceB)

deriving instance FromJSON (HKDTwo StateF)

deriving instance ToJSON (HKDTwo StateF)

deriving via (Generically (HKDTwo StateF)) instance Semigroup (HKDTwo StateF)

deriving via (Generically (HKDTwo StateF)) instance Monoid (HKDTwo StateF)

--------------------------------------------------------------------------------

sequenceHKDSer :: HKDTwo (PhantomState (Flip TextSerializer)) -> TextSerializer (HKDTwo OutputF) (HKDTwo InputF)
sequenceHKDSer (HKDTwo (PhantomState (Flip s1)) (PhantomState (Flip s2))) =
  Serializer parser' printer'
  where
    parser' :: Text -> Maybe (HKDTwo InputF)
    parser' x =
      case (s1.parser x, s2.parser x) of
        (Nothing, Nothing) -> Nothing
        (Just (), Nothing) -> Just $ HKDTwo (InputF (Just ())) (InputF Nothing)
        (Nothing, Just i2) -> Just $ HKDTwo (InputF Nothing) (InputF (Just i2))
        (Just (), Just i2) -> Just $ HKDTwo (InputF (Just ())) (InputF (Just i2))

    printer' :: HKDTwo OutputF -> Text
    printer' = \case
      HKDTwo (OutputF Nothing) (OutputF Nothing) -> mempty
      HKDTwo (OutputF (Just o1)) (OutputF Nothing) -> s1.printer o1
      HKDTwo (OutputF Nothing) (OutputF (Just o2)) -> s2.printer o2
      HKDTwo (OutputF (Just o1)) (OutputF (Just o2)) -> s1.printer o1 <> "\n" <> s2.printer o2

--------------------------------------------------------------------------------

hkdBot :: HKDTwo (Bot IO)
hkdBot = HKDTwo helloBot (sessionize mempty calculatorBot)

hkdSer :: HKDTwo (PhantomState (Flip TextSerializer))
hkdSer = HKDTwo (PhantomState $ Flip helloBotSerializer) (PhantomState $ Flip $ sessionSerializer calculatorSerializer)

-- | After sequencing we can smoosh them into our final bot ready to run:
appliedHKDBot :: Bot IO (HKDTwo StateF) Text Text
appliedHKDBot = applySerializer (sequenceB hkdBot) (sequenceHKDSer hkdSer)
