{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module CofreeBot.Utils.ListT
  ( -- * ListF
    ListF (..),

    -- * ListT
    ListT (..),
    emptyListT,
    consListT,
    singletonListT,
    joinListT,
    toListT,
    fromListT,
    hoistListT,
    interleaveListT,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable
import Data.Functor ((<&>))
import Data.These

data ListF a r = NilF | ConsF a r
  deriving (Functor)

instance Bifunctor ListF where
  bimap f g = \case
    NilF -> NilF
    ConsF a r -> ConsF (f a) (g r)

newtype ListT m a = ListT
  { runListT :: m (ListF a (ListT m a))
  }

instance Functor m => Functor (ListT m) where
  fmap f (ListT ma) = ListT $ fmap (bimap f (fmap f)) $ ma

instance Monad m => Applicative (ListT m) where
  pure = return
  (<*>) = ap

instance Monad m => Alternative (ListT m) where
  empty = emptyListT
  ListT m <|> ListT n = ListT $ do
    x <- m
    y <- n
    pure $ case (x, y) of
      (NilF, NilF) -> NilF
      (ConsF x' xs, NilF) -> ConsF x' xs
      (NilF, ConsF y' ys) -> ConsF y' ys
      (ConsF x' xs, ConsF y' ys) ->
        ConsF x' (ListT $ pure $ ConsF y' (xs <|> ys))

instance MonadTrans ListT where
  lift ma = ListT $ fmap (\a -> ConsF a (ListT $ pure NilF)) ma

instance MonadIO m => MonadIO (ListT m) where
  liftIO io = ListT $ liftIO $ fmap (\a -> ConsF a (ListT $ pure NilF)) io

instance MonadError e m => MonadError e (ListT m) where
  throwError = lift . throwError

  -- catchError m f = ListT $ runListT m `catchError` \e -> runListT (f e)
  catchError m f = ListT . deepCatch . runListT $ m
    where
      deepCatch m' = fmap deepCatch' m' `catchError` \e -> runListT (f e)

      deepCatch' = \case
        NilF -> NilF
        ConsF a r -> ConsF a (ListT $ deepCatch $ runListT r)

emptyListT :: Applicative m => ListT m a
emptyListT = ListT $ pure NilF

consListT :: Applicative m => a -> ListT m a -> ListT m a
consListT a = \case
  ListT ml ->
    ListT $
      ml <&> \case
        NilF -> ConsF a emptyListT
        ConsF x xs -> ConsF a $ ListT $ pure $ ConsF x xs

singletonListT :: Applicative m => a -> ListT m a
singletonListT a = consListT a emptyListT

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

instance Monad m => Monad (ListT m) where
  return = ListT . return . (`ConsF` emptyListT)
  ma >>= amb = joinListT $ fmap amb ma

toListT :: (Foldable t, Applicative m) => t a -> ListT m a
toListT = foldr' consListT emptyListT

hoistListT :: Functor n => (forall x. m x -> n x) -> ListT m a -> ListT n a
hoistListT f = ListT . fmap (fmap (hoistListT f)) . f . runListT

fromListT :: Monad m => ListT m a -> m [a]
fromListT (ListT m) =
  m >>= \case
    NilF -> pure []
    ConsF a xs -> fmap (a :) $ fromListT xs

interleaveListT :: Monad m => ListT m a -> ListT m b -> ListT m (These a b)
interleaveListT (ListT m) (ListT n) = ListT $ do
  x <- m
  y <- n
  pure $ case (x, y) of
    (NilF, NilF) -> NilF
    (ConsF x' xs, NilF) -> ConsF (This x') (fmap This xs)
    (NilF, ConsF y' ys) -> ConsF (That y') (fmap That ys)
    (ConsF x' xs, ConsF y' ys) -> ConsF (These x' y') (interleaveListT xs ys)
