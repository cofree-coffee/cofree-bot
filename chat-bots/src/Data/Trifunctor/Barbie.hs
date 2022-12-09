{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Trifunctor.Barbie where

-- ( First (..),
--   Second (..),
--   Optional (..),
--   traverseThese,
--   zipMyApp,
-- )

--------------------------------------------------------------------------------

import Data.Bifunctor.Const2 (Const2 (..))
import Data.Bifunctor.Product (Product (..))
import Data.Chat.Bot (Bot)
import Data.Chat.Serialization (TextSerializer)
import Data.Chat.Serialization qualified as S
import Data.Chat.Utils (type (/+\))
import Data.Kind (Type)
import Data.Profunctor (Profunctor (..))
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

class FunctorB2 (b :: (k -> k' -> Type) -> Type) where
  b2map :: (forall x y. f x y -> g x y) -> b f -> b g

instance FunctorB2 MyApp where
  b2map :: (forall x y. f x y -> g x y) -> MyApp f -> MyApp g
  b2map nat MyApp {..} = MyApp {helloBot' = nat helloBot', coinFlipBot' = nat coinFlipBot'}

--------------------------------------------------------------------------------

class FunctorB2 b => ApplicativeB2 (b :: (Type -> Type -> Type) -> Type) where
  b2pure :: Profunctor f => f () () -> b f
  b2prod :: b f -> b g -> b (f `Product` g)

instance ApplicativeB2 MyApp where
  b2pure :: Profunctor f => f () () -> MyApp f
  b2pure fxy = MyApp (lmap (const ()) fxy) (lmap (const ()) fxy)

  b2prod :: MyApp f -> MyApp g -> MyApp (Product f g)
  b2prod (MyApp x1 y1) (MyApp x2 y2) = MyApp (Pair x1 x2) (Pair y1 y2)

--------------------------------------------------------------------------------

class FunctorB2 b => TraversableB2 (b :: (k -> k' -> Type) -> Type) where
  b2traverse :: Applicative e => (forall x y. f x y -> e (g x y)) -> b f -> e (b g)

instance TraversableB2 MyApp where
  b2traverse :: Applicative e => (forall x y. f x y -> e (g x y)) -> MyApp f -> e (MyApp g)
  b2traverse f MyApp {..} = MyApp <$> f helloBot' <*> f coinFlipBot'

--------------------------------------------------------------------------------

newtype First a b = First (Maybe a)

newtype Second a b = Second (Maybe b)

newtype Optional p a b = Both (Maybe (p a b))

--------------------------------------------------------------------------------

-- TODO: Replace @align2'@ with @Semialign2@ instance.
traverseThese ::
  ( Profunctor p,
    Applicative (p (MyApp First))
  ) =>
  (forall a b c d. p a b -> p c d -> p (a /+\ c) (b /+\ d)) ->
  MyApp p ->
  p (MyApp First) (MyApp Second)
traverseThese align2' barbie = b2traverse _ barbie

b2ZipWith :: ApplicativeB2 barbie => (forall x y. p x y -> q x y -> pq x y) -> barbie p -> barbie q -> barbie pq
b2ZipWith f bp bq = b2map (\(Pair fa ga) -> f fa ga) (bp `b2prod` bq)

--------------------------------------------------------------------------------

-- | Convert a 'FunctorB' into a 'FunctorT' and vice-versa.
newtype Flip b l r = Flip {runFlip :: b r l}
  deriving (Eq, Ord, Read, Show)

--------------------------------------------------------------------------------
-- HKD Bot Proof of Concept

-- TODO: Add a state param
data MyApp p = MyApp
  { helloBot' :: p Text (),
    coinFlipBot' :: p Bool ()
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

-- | Packing the HKD with 'Serializer' gives us serializers for the
-- 'Bot' subroutines of our HKD.
myAppSerializer :: MyApp TextSerializer
myAppSerializer =
  MyApp
    { helloBot' = S.Serializer (\t -> if t == "cofree-bot" then Just () else Nothing) id,
      coinFlipBot' = S.Serializer (\t -> if t == "flip a coin" then Just () else Nothing) (Text.pack . show)
    }

-- | 'Serializer' subroutine of 'MyApp' augmented with label prefixes
-- for each subroutine.
myAppSerializer' :: MyApp TextSerializer
myAppSerializer' = b2ZipWith (\(Const2 x) -> S.prefix x) myAppNames myAppSerializer

-- | By traversing an HKD of 'Serializer' we can produce an actual
-- 'Serializer'.
actualSerializer :: TextSerializer (MyApp First) (MyApp Second)
actualSerializer = undefined (S./+\) myAppSerializer'

actualSerializer' :: MyApp TextSerializer -> TextSerializer (MyApp First) (MyApp Second)
actualSerializer' (MyApp (S.Serializer par1 pri1) (S.Serializer par2 pri2)) = S.Serializer parser printer
  where
    parser :: Text -> Maybe (MyApp Second)
    parser input =
      case (par1 input, par2 input) of
        (Nothing, Nothing) -> Nothing
        (Just x, Nothing) -> Just $ MyApp (Second (Just x)) (Second Nothing)
        (Nothing, Just y) -> Just $ MyApp (Second Nothing) (Second (Just y))
        (Just x, Just y) -> Just $ MyApp (Second (Just x)) (Second (Just y))

    printer :: MyApp First -> Text
    printer (MyApp (First Nothing) (First Nothing)) = mempty
    printer (MyApp (First (Just x)) (First Nothing)) = pri1 x
    printer (MyApp (First Nothing) (First (Just y))) = pri2 y
    printer (MyApp (First (Just x)) (First (Just y))) = pri1 x <> "\n" <> pri2 y

-- | Packing the HKD with 'Bot' gives us the bot subroutinesf for our
-- HKD.
myAppBot :: Monad m => MyApp (Flip (Bot m s))
myAppBot =
  MyApp
    { helloBot' = Flip undefined,
      coinFlipBot' = Flip undefined
    }

-- NOTE: This doesn't typecheck at the moment due to the state @s@
-- getting tensored.
--
-- -- | By traversing the HKD of 'Bot' we can produce an actual 'Bot'.
-- actualBot :: Bot m (s /\ s') (MyApp Second) (MyApp First)
-- actualBot = runFlip $ traverseThese (\(Flip x) (Flip y) -> Flip (x M./+\ y)) myAppBot

-- | Sample serialized output from 'actualBot'.
exampleOutput :: MyApp First
exampleOutput =
  MyApp
    { helloBot' = First $ Just "you talking to me punk?",
      coinFlipBot' = First Nothing
    }
