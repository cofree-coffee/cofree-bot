module CofreeBot.Utils
  ( -- * Product
    type (/\)
  , pattern (:&)
  , (|*|)
  , type (/+\)
  ,
    -- * Coproduct
    type (\/)
  ,

    -- * Wedge Product
    type (\*/)
  ,
    -- *  MTL Helpers
    Transformers
  , duplicate
  , indistinct
  ,
    -- * Misc
    distinguish
  , PointedChoice(..)
  ) where

-------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Arrow                  ( (&&&) )
import           Data.Kind
import           Data.These                     ( These )

-------------------------------------------------------------------------------

type (/\) = (,)

infixr /\

pattern (:&) :: a -> b -> (a, b)
pattern a :& b = (a, b)

{-# COMPLETE (:&) #-}

infixr :&

(|*|) :: Applicative f => f a -> f b -> f (a /\ b)
(|*|) = liftA2 (,)

infixr |*|


-------------------------------------------------------------------------------

type (\/) = Either

infixr \/

-------------------------------------------------------------------------------

type a \*/ b = Maybe (Either a b)

infixr \*/

-------------------------------------------------------------------------------
-- These
-------------------------------------------------------------------------------

type a /+\ b = These a b

infixr /+\

-------------------------------------------------------------------------------

type Transformers
  :: [(Type -> Type) -> Type -> Type]
  -> (Type -> Type) -> Type -> Type
type family Transformers ts m
  where
  Transformers '[] m = m
  Transformers (t ': ts) m = t (Transformers ts m)

duplicate :: x -> (x, x)
duplicate = id &&& id

indistinct :: Either x x -> x
indistinct = id `either` id

--------------------------------------------------------------------------------

distinguish :: (a -> Bool) -> a -> Either a a
distinguish f x | f x       = Right x
                | otherwise = Left x

class PointedChoice p where
  pleft :: p a b -> p (x \*/ a) (x \*/ b)
  pright :: p a b -> p (a \*/ x) (b \*/ x)
