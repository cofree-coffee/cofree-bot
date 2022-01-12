module CofreeBot.Utils where

import           Control.Applicative
import           Data.Kind

-------------------------------------------------------------------------------
-- Tensors
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Product
-------------------------------------------------------------------------------

type (/\) = (,)

infixr /\

pattern (:&) :: a -> b -> (a, b)
pattern a :& b = (a, b)

{-# COMPLETE (:&) #-}

infixr :&

-------------------------------------------------------------------------------
-- Coproduct
-------------------------------------------------------------------------------

type (\/) = Either

infixr \/

-------------------------------------------------------------------------------
-- Wedge product
-------------------------------------------------------------------------------

type a \?/ b = Maybe (Either a b)

infixr \?/

-------------------------------------------------------------------------------
-- Monoidal functor combinators
-------------------------------------------------------------------------------

(|*|) :: Applicative f => f a -> f b -> f (a, b)
(|*|) = liftA2 (,)

infixr |*|

type Transformers
  :: [(Type -> Type) -> Type -> Type]
  -> (Type -> Type) -> Type -> Type
type family Transformers ts m
  where
  Transformers '[] m = m
  Transformers (t ': ts) m = t (Transformers ts m)

indistinct :: Either x x -> x
indistinct = either id id

distinguish :: (a -> Bool) -> a -> Either a a
distinguish f x | f x       = Right x
                | otherwise = Left x
