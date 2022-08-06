{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module CofreeBot.Utils.ListT where

import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (foldr')
import Control.Monad (ap)
import Data.Functor ((<&>))

data ListF a r = NilF | ConsF a r
  deriving Functor

instance Bifunctor ListF
  where
  bimap f g = \case
    NilF -> NilF
    ConsF a r -> ConsF (f a) (g r)

newtype ListT m a = ListT
  { runListT :: m (ListF a (ListT m a))
  }

instance Functor m => Functor (ListT m)
  where
  fmap f (ListT ma) = ListT $ fmap (bimap f (fmap f)) $ ma

instance Monad m => Applicative (ListT m)
  where
  pure = return
  (<*>) = ap

emptyListT :: Applicative m => ListT m a
emptyListT = ListT $ pure NilF

consListT :: Applicative m => a -> ListT m a -> ListT m a
consListT a = \case
  ListT ml -> ListT $ ml <&> \case
    NilF -> ConsF a emptyListT
    ConsF x xs -> ConsF a $ ListT $ pure $ ConsF x xs

joinListT :: Monad m => ListT m (ListT m a) -> ListT m a
joinListT (ListT ma) = ListT $ do
  fma <- ma
  case fma of
    NilF -> return NilF
    ListT mxs `ConsF` xss -> do
      xs <- mxs
      case xs of
        NilF -> runListT $ joinListT xss
        x `ConsF` xs' -> runListT $ consListT x $ joinListT $ consListT xs' xss

instance Monad m => Monad (ListT m)
  where
  return = ListT . return . (`ConsF` emptyListT)
  ma >>= amb = joinListT $ fmap amb ma

toListT :: (Foldable t, Applicative m) => t a -> ListT m a
toListT = foldr' consListT emptyListT
