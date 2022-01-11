{-# LANGUAGE PatternSynonyms #-}

module CofreeBot.Utils where

import           Control.Applicative
import           Data.Kind

(|*|) :: Applicative f => f a -> f b -> f (a, b)
(|*|) = liftA2 (,)

infixr 9 |*|

type (/\) = (,)

infixr 9 /\

type (\/) = Either

infixr 9 \/

type a \?/ b = Maybe (Either a b)

pattern (:&) :: a -> b -> (a, b)
pattern a :& b = (a, b)

{-# COMPLETE (:&) #-}

infixr 9 :&

type Transformers ::
  [(Type -> Type) -> Type -> Type] ->
  (Type -> Type) ->
  Type ->
  Type
type family Transformers ts m where
  Transformers '[] m = m
  Transformers (t ': ts) m = t (Transformers ts m)

same :: Either x x -> x
same = either id id
