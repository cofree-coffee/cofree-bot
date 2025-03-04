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
import Data.Chat.Bot.Serialization (TextSerializer, Serializer (..))
import Data.Profunctor (dimap)

--------------------------------------------------------------------------------

newtype SerIn o i = SerIn i
newtype SerOut o i = SerOut o

class SequenceSer f where
  sequenceSer :: f TextSerializer -> TextSerializer (f SerOut) (f SerIn)
  default sequenceSer ::
      ( forall x. Generic (f x),
        GSequenceSer (Rep (f TextSerializer)) (Rep (f SerOut)) (Rep (f SerIn))) =>
      f TextSerializer -> TextSerializer (f SerOut) (f SerIn)
  sequenceSer s =
      dimap _ _ $ gsequenceSer $ from s

type GSequenceSer :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Constraint
class GSequenceSer fbrep irep orep where
  gsequenceSer :: fbrep x -> TextSerializer (irep x) (orep x)

instance
  (GSequenceSer f i o) =>
  GSequenceSer
    (M1 _1 _2 f)
    (M1 _1 _2 i)
    (M1 _1 _2 o)
  where
  gsequenceSer (M1 b) = dimap unM1 M1 $ gsequenceSer b

instance
  forall f1 i1 o1 f2 i2 o2.
  (GSequenceSer f1 i1 o1, GSequenceSer f2 i2 o2) =>
  GSequenceSer
    (f1 :*: f2)
    (i1 :*: i2)
    (o1 :*: o2)
  where
  gsequenceSer (s1 :*: s2) =
    let Serializer {parser = parse1, printer = print1} = gsequenceSer @f1 @i1 @o1 s1
        Serializer {parser = parse2, printer = print2} = gsequenceSer @f2 @i2 @o2 s2
     in Serializer
      { parser = \i -> liftA2 (:*:) (parse1 i) (parse2 i),
        printer = \(o1 :*: o2) -> print1 o1 <> "\n" <> print2 o2
      }

instance
  GSequenceSer
    (K1 _1 (TextSerializer o i))
    (K1 _1 (SerOut o i))
    (K1 _1 (SerIn o i))
  where
  gsequenceSer (K1 (Serializer parse print)) =
    Serializer
      { parser = fmap (K1 . SerIn) . parse,
        printer = \(K1 (SerOut o)) -> print o
      }

--------------------------------------------------------------------------------

newtype StateF s i o = StateF {getStateF :: s}
  deriving newtype (Semigroup, Monoid, FromJSON, ToJSON)

newtype InputF s i o = InputF {getInputF :: Maybe i}

newtype OutputF s i o = OutputF {getOutputF :: Maybe o}

type ProKind = Type -> Type -> Type -> Type

type BotKind = ProKind -> Type

type SequenceB :: BotKind -> Constraint
class SequenceB f where
  sequenceB :: forall m. (Monad m) => f (Bot m) -> Bot m (f StateF) (f InputF) (f OutputF)
  default sequenceB ::
    forall m.
    ( forall x. Generic (f x),
      GSequenceB m (Rep (f (Bot m))) (Rep (f StateF)) (Rep (f InputF)) (Rep (f OutputF)),
      Monad m
    ) =>
    f (Bot m) ->
    Bot m (f StateF) (f InputF) (f OutputF)
  sequenceB b =
    mapBot (to, from) from to
      $ gsequenceB
        @m
        @(Rep (f (Bot m)))
        @(Rep (f StateF))
        @(Rep (f InputF))
        @(Rep (f OutputF))
      $ from b

type GSequenceB :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Constraint
class (Monad m) => GSequenceB m fbrep srep irep orep where
  gsequenceB :: fbrep x -> Bot m (srep x) (irep x) (orep x)

instance
  (GSequenceB m f s i o) =>
  GSequenceB
    m
    (M1 _1 _2 f)
    (M1 _1 _2 s)
    (M1 _1 _2 i)
    (M1 _1 _2 o)
  where
  gsequenceB (M1 b) = mapBot (M1, unM1) unM1 M1 $ gsequenceB b

instance
  (GSequenceB m f1 s1 i1 o1, GSequenceB m f2 s2 i2 o2) =>
  GSequenceB
    m
    (f1 :*: f2)
    (s1 :*: s2)
    (i1 :*: i2)
    (o1 :*: o2)
  where
  gsequenceB (b1 :*: b2) =
    Bot $ \(s1 :*: s2) (i1 :*: i2) ->
      liftA2
        (\(o1, s1') (o2, s2') -> ((o1 :*: o2), (s1' :*: s2')))
        (runBot (gsequenceB b1) s1 i1)
        (runBot (gsequenceB b2) s2 i2)

instance
  (Monad m) =>
  GSequenceB
    m
    (K1 _1 (Bot m s i o))
    (K1 _1 (StateF s i o))
    (K1 _1 (InputF s i o))
    (K1 _1 (OutputF s i o))
  where
  gsequenceB (K1 b) =
    Bot $ \(K1 (StateF s)) (K1 (InputF mi)) ->
      case mi of
        Just i -> fmap (K1 . OutputF . Just *** K1 . StateF) $ runBot b s i
        Nothing -> pure (K1 (OutputF Nothing), K1 $ StateF s)
