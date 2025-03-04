{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Data.Chat.Bot.HKD where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Arrow
import Data.Aeson (FromJSON, ToJSON)
import Data.Chat.Bot
import Data.Kind
import GHC.Generics
import Prelude

--------------------------------------------------------------------------------

newtype StateF s i o = StateF {getStateF :: s}
  deriving newtype (Semigroup, Monoid, FromJSON, ToJSON)

newtype InputF s i o = InputF {getInputF :: Maybe i}

newtype OutputF s i o = OutputF {getOutputF :: Maybe o}

type ProKind = Type -> Type -> Type -> Type

type BotKind = ProKind -> Type

type SequenceBot :: BotKind -> Constraint
class SequenceBot f where
  sequenceBot :: forall m. (Monad m) => f (Bot m) -> Bot m (f StateF) (f InputF) (f OutputF)
  default sequenceBot ::
    forall m.
    ( forall x. Generic (f x),
      GSequenceBot m (Rep (f (Bot m))) (Rep (f StateF)) (Rep (f InputF)) (Rep (f OutputF)),
      Monad m
    ) =>
    f (Bot m) ->
    Bot m (f StateF) (f InputF) (f OutputF)
  sequenceBot b =
    mapBot (to, from) from to
      $ gsequenceBot
        @m
        @(Rep (f (Bot m)))
        @(Rep (f StateF))
        @(Rep (f InputF))
        @(Rep (f OutputF))
      $ from b

type GSequenceBot :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Constraint
class (Monad m) => GSequenceBot m fbrep srep irep orep where
  gsequenceBot :: fbrep x -> Bot m (srep x) (irep x) (orep x)

instance (GSequenceBot m f s i o) => GSequenceBot m (M1 _1 _2 f) (M1 _1 _2 s) (M1 _1 _2 i) (M1 _1 _2 o) where
  gsequenceBot :: M1 _1 _2 f x -> Bot m (M1 _1 _2 s x) (M1 _1 _2 i x) (M1 _1 _2 o x)
  gsequenceBot (M1 b) = mapBot (M1, unM1) unM1 M1 $ gsequenceBot b

instance (GSequenceBot m f1 s1 i1 o1, GSequenceBot m f2 s2 i2 o2) => GSequenceBot m (f1 :*: f2) (s1 :*: s2) (i1 :*: i2) (o1 :*: o2) where
  gsequenceBot :: (:*:) f1 f2 x -> Bot m ((:*:) s1 s2 x) ((:*:) i1 i2 x) ((:*:) o1 o2 x)
  gsequenceBot (b1 :*: b2) =
    Bot $ \(s1 :*: s2) (i1 :*: i2) ->
      liftA2
        (\(o1, s1') (o2, s2') -> ((o1 :*: o2), (s1' :*: s2')))
        (runBot (gsequenceBot b1) s1 i1)
        (runBot (gsequenceBot b2) s2 i2)

instance (Monad m) => GSequenceBot m (K1 _1 (Bot m s i o)) (K1 _1 (StateF s i o)) (K1 _1 (InputF s i o)) (K1 _1 (OutputF s i o)) where
  gsequenceBot :: K1 _1 (Bot m s i o) x -> Bot m (K1 _1 (StateF s i o) x) (K1 _1 (InputF s i o) x) (K1 _1 (OutputF s i o) x)
  gsequenceBot (K1 b) =
    Bot $ \(K1 (StateF s)) (K1 (InputF mi)) ->
      case mi of
        Just i -> fmap (K1 . OutputF . Just *** K1 . StateF) $ runBot b s i
        Nothing -> pure (K1 (OutputF Nothing), K1 $ StateF s)
