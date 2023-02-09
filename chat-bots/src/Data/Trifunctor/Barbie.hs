{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}

module Data.Trifunctor.Barbie where

-- ( First (..),
--   Second (..),
--   Optional (..),
--   traverseThese,
--   zipMyApp,
-- )

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Category.Cartesian
import Data.Align2
import Data.Bifoldable
import Data.Bifunctor.Const2 (Const2 (..))
import Data.Bifunctor.Monoidal
import Data.Bifunctor.Monoidal.Specialized
import Data.Bifunctor.Product (Product (..))
import Data.Bitraversable
import Data.Bool
import Data.Chat.Bot (Bot)
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.Chat.Utils (type (/+\), can)
import Data.Kind (Type)
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Data.These
import Data.Functor.Compose
import Data.Functor.Identity

--------------------------------------------------------------------------------

class FunctorB2 (b :: (k -> k' -> Type) -> Type) where
  b2map :: (forall x y. f x y -> g x y) -> b f -> b g

class FunctorB2 b => ApplicativeB2 (b :: (Type -> Type -> Type) -> Type) where
  b2pure :: Profunctor f => (forall x y. f x y) -> b f
  b2prod :: b f -> b g -> b (f `Product` g)

class FunctorB2 b => TraversableB2 (b :: (k -> k' -> Type) -> Type) where
  b2traverse :: Applicative e => (forall x y. f x y -> e (g x y)) -> b f -> e (b g)

--------------------------------------------------------------------------------

newtype First a b = First (Maybe a)

newtype Second a b = Second (Maybe b)

newtype Both p a b = Both (Maybe (p a b))

--------------------------------------------------------------------------------

-- TODO: Replace @align2'@ with @Semialign2@ instance.
traverseThese ::
  ( Profunctor p,
    Semialign2 p,
    Applicative (p (MyApp First))
  ) =>
  (forall a b c d. p a b -> p c d -> p (a /+\ c) (b /+\ d)) ->
  MyApp p ->
  p (MyApp First) (MyApp Second)
traverseThese align2' barbie = b2traverse undefined barbie

b2ZipWith :: ApplicativeB2 barbie => (forall x y. p x y -> q x y -> pq x y) -> barbie p -> barbie q -> barbie pq
b2ZipWith f bp bq = b2map (\(Pair fa ga) -> f fa ga) (bp `b2prod` bq)

--------------------------------------------------------------------------------

newtype Flip b l r = Flip {runFlip :: b r l}
  deriving (Eq, Ord, Read, Show)

instance Semigroupal (->) (,) (,) (,) f => Semigroupal (->) (,) (,) (,) (Flip f) where
  combine :: Semigroupal (->) (,) (,) (,) f => (Flip f x y, Flip f x' y') -> Flip f (x, x') (y, y')
  combine (Flip f1, Flip f2) = Flip $ combine (f1, f2)

--------------------------------------------------------------------------------
-- HKD Bot Proof of Concept

-- TODO: Add a state param
data MyApp p = MyApp
  { helloBot' :: p () Text,
    coinFlipBot' :: p () Bool
  }

instance FunctorB2 MyApp where
  b2map :: (forall x y. f x y -> g x y) -> MyApp f -> MyApp g
  b2map nat MyApp {..} = MyApp {helloBot' = nat helloBot', coinFlipBot' = nat coinFlipBot'}

instance ApplicativeB2 MyApp where
  b2pure :: Profunctor f => (forall x y. f x y) -> MyApp f
  b2pure fxy = MyApp fxy fxy

  b2prod :: MyApp f -> MyApp g -> MyApp (Product f g)
  b2prod (MyApp x1 y1) (MyApp x2 y2) = MyApp (Pair x1 x2) (Pair y1 y2)

instance TraversableB2 MyApp where
  b2traverse :: Applicative e => (forall x y. f x y -> e (g x y)) -> MyApp f -> e (MyApp g)
  b2traverse f MyApp {..} = MyApp <$> f helloBot' <*> f coinFlipBot'

--------------------------------------------------------------------------------

helloBot :: Monad m => Bot m s () Text
helloBot = undefined

coinFlipBot :: Bot IO () () Bool
coinFlipBot = undefined

-- | Packing the HKD with 'Bot' gives us the bot subroutinesf for our
-- HKD.
myAppBot :: Monad m => MyApp (Bot m s)
myAppBot =
  MyApp
    { helloBot' = helloBot,
      coinFlipBot' = undefined
    }

helloBotSerializer :: TextSerializer Text ()
helloBotSerializer = undefined

coinFlipSerializer :: TextSerializer Bool ()
coinFlipSerializer = undefined

-- | Packing the HKD with 'Serializer' gives us serializers for the
-- 'Bot' subroutines of our HKD.
myAppSerializer :: MyApp (Flip TextSerializer)
myAppSerializer =
  MyApp
    { helloBot' = Flip helloBotSerializer,
      coinFlipBot' = Flip coinFlipSerializer
    }

-- | Packing the HKD with 'Const2 Text' gives us labels for the 'Bot'
-- subroutines of our HKD. This could be constructed with GHC Generics
-- or Template Haskell.
myAppNames :: MyApp (Const2 Text)
myAppNames =
  MyApp
    { helloBot' = "Hello Bot",
      coinFlipBot' = "Coin Flip Bot"
    }

--------------------------------------------------------------------------------

instance Show (HKD2 In) where
  show (HKD2 x y) = "(HKD2 " <> show x <> " " <> show y <> ")"

instance Show (HKD2 Out) where
  show (HKD2 x y) = "(HKD2 " <> show x <> " " <> show y <> ")"

sequenceHKD2 :: Semigroupal (->) t1 t2 (,) p => HKD2 p -> p (t1 () ()) (t2 Bool Text)
sequenceHKD2 (HKD2 a b) = combine (a, b)

data HKD2 f = HKD2 {a :: f () Bool, b :: f () Text}

instance Semigroupal (->) These These (,) (Flip TextSerializer) where
  combine :: (Flip TextSerializer x y, Flip TextSerializer x' y') -> Flip TextSerializer (These x x') (These y y')
  combine (Flip (S.Serializer par1 pri1), Flip (S.Serializer par2 pri2)) =
    Flip $
      S.Serializer
        { parser = uncurry can . (par1 &&& par2),
          printer = these pri1 pri2 (\y y' -> pri1 y <> pri2 y')
        }

type Optional :: ((k -> Type) -> Type) -> (k -> Type) -> Type
newtype Optional hkd f = Optional (hkd (Compose Maybe f))


data In a b = In (Maybe a)
  deriving (Show)

data Out a b = Out (Maybe b)
  deriving (Show)


sequenceSerializer :: HKD2 (Flip TextSerializer) -> TextSerializer (HKD2 Out) (HKD2 In)
sequenceSerializer (HKD2 x y) = f $ combine (x, y)
  where
    f :: Flip TextSerializer (These () ()) (These Bool Text) -> TextSerializer (HKD2 Out) (HKD2 In)
    f (Flip (S.Serializer par pri)) =
      S.Serializer
        { parser = fmap (these (\() -> HKD2 (In (Just ())) (In Nothing)) (\() -> HKD2 (In Nothing) (In (Just ()))) (\() () -> HKD2 (In (Just ())) (In (Just ())))) . par,
          printer = \case
            (HKD2 (Out Nothing) (Out Nothing)) -> mempty
            (HKD2 (Out (Just a)) (Out Nothing)) -> pri $ This a
            (HKD2 (Out Nothing) (Out (Just b))) -> pri $ That b
            (HKD2 (Out (Just a)) (Out (Just b))) -> pri $ These a b
        }

serializerHKD :: HKD2 (Flip TextSerializer)
serializerHKD =
  HKD2
    { a = Flip S.Serializer {parser = bool Nothing (Just ()) . (== "flip a coin"), printer = bool "tails" "heads"},
      b = Flip S.Serializer {parser = bool Nothing (Just ()) . (== "cofree-bot"), printer = id}
    }

serializer :: TextSerializer (HKD2 Out) (HKD2 In)
serializer = sequenceSerializer serializerHKD


data In1 a = In1 a
data Out1 a = Out1 a

--sequencer :: Semigroupal (->) (/+\) (/+\) (,) p => b p -> p (Optional b In) (Optional b Out)
--sequencer = undefined
