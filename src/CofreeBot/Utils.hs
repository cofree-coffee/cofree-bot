module CofreeBot.Utils where

import           Control.Applicative
import           Control.Arrow                  ( (&&&) )
import           Data.Kind
import           Data.These                     ( These )

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

type a \*/ b = Maybe (Either a b)

infixr \*/

-------------------------------------------------------------------------------
-- These
-------------------------------------------------------------------------------

type a /+\ b = These a b

infixr /+\

-------------------------------------------------------------------------------
-- Monoidal functor combinators
-------------------------------------------------------------------------------

(|*|) :: Applicative f => f a -> f b -> f (a, b)
(|*|) = liftA2 (,)

infixr |*|

-------------------------------------------------------------------------------
-- MTL stuff
-------------------------------------------------------------------------------

type Transformers
  :: [(Type -> Type) -> Type -> Type]
  -> (Type -> Type) -> Type -> Type
type family Transformers ts m
  where
  Transformers '[] m = m
  Transformers (t ': ts) m = t (Transformers ts m)

-------------------------------------------------------------------------------
-- MTL stuff
-------------------------------------------------------------------------------

duplicate :: x -> (x, x)
duplicate = id &&& id

indistinct :: Either x x -> x
indistinct = id `either` id

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

distinguish :: (a -> Bool) -> a -> Either a a
distinguish f x | f x       = Right x
                | otherwise = Left x

class PointedChoice p where
  pleft :: p a b -> p (x \*/ a) (x \*/ b)
  pright :: p a b -> p (a \*/ x) (b \*/ x)
