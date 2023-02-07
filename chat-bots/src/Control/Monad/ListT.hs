{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ListT
  ( -- * ListT
    ListT (..),
    ListF (..),

    -- * Operations
    emptyListT,
    consListT,
    singletonListT,
    joinListT,
    toListT,
    fromListT,
    hoistListT,
    alignListT,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Alternative (..))
import Control.Monad.Except (MonadError (..), MonadIO (..), MonadTrans (..), ap)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.These (These (..))

--------------------------------------------------------------------------------

-- | ListT done right, see https://www.haskell.org/haskellwiki/ListT_done_right_alternative
--
-- NOTE: There are several other encodings available on hackage. This
-- particular version best fit our use case.
newtype ListT m a = ListT
  { runListT :: m (ListF a (ListT m a))
  }

instance Functor m => Functor (ListT m) where
  fmap :: Functor m => (a -> b) -> ListT m a -> ListT m b
  fmap f (ListT ma) = ListT $ fmap (bimap f (fmap f)) $ ma

instance Monad m => Applicative (ListT m) where
  pure :: Monad m => a -> ListT m a
  pure = ListT . return . (`ConsF` emptyListT)

  (<*>) :: Monad m => ListT m (a -> b) -> ListT m a -> ListT m b
  (<*>) = ap

instance Monad m => Alternative (ListT m) where
  empty :: Monad m => ListT m a
  empty = emptyListT

  (<|>) :: Monad m => ListT m a -> ListT m a -> ListT m a
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
  lift :: Monad m => m a -> ListT m a
  lift ma = ListT $ fmap (\a -> ConsF a (ListT $ pure NilF)) ma

instance MonadIO m => MonadIO (ListT m) where
  liftIO :: MonadIO m => IO a -> ListT m a
  liftIO io = ListT $ liftIO $ fmap (\a -> ConsF a (ListT $ pure NilF)) io

instance MonadError e m => MonadError e (ListT m) where
  throwError :: MonadError e m => e -> ListT m a
  throwError = lift . throwError

  catchError :: MonadError e m => ListT m a -> (e -> ListT m a) -> ListT m a
  catchError m f = ListT . deepCatch . runListT $ m
    where
      deepCatch m' = fmap deepCatch' m' `catchError` \e -> runListT (f e)

      deepCatch' = \case
        NilF -> NilF
        ConsF a r -> ConsF a (ListT $ deepCatch $ runListT r)

instance Monad m => Monad (ListT m) where
  return :: Monad m => a -> ListT m a
  return = pure

  (>>=) :: Monad m => ListT m a -> (a -> ListT m b) -> ListT m b
  ma >>= amb = joinListT $ fmap amb ma

data ListF a r = NilF | ConsF a r
  deriving (Functor)

instance Bifunctor ListF where
  bimap :: (a -> b) -> (c -> d) -> ListF a c -> ListF b d
  bimap f g = \case
    NilF -> NilF
    ConsF a r -> ConsF (f a) (g r)

--------------------------------------------------------------------------------

-- | The empty 'ListT'.
emptyListT :: Applicative m => ListT m a
emptyListT = ListT $ pure NilF

-- | A 'ListT' of one element.
singletonListT :: Applicative m => a -> ListT m a
singletonListT a = consListT a emptyListT

-- | Consing a value to a 'LisT'.
consListT :: Applicative m => a -> ListT m a -> ListT m a
consListT a = \case
  ListT ml ->
    ListT $
      ml <&> \case
        NilF -> ConsF a emptyListT
        ConsF x xs -> ConsF a $ ListT $ pure $ ConsF x xs

-- | Convert some 'Foldable' @t@ into a 'ListT'.
toListT :: (Foldable t, Applicative m) => t a -> ListT m a
toListT = foldr' consListT emptyListT

-- | Convert a 'ListT' into a '[]' and sequence the effects.
fromListT :: Monad m => ListT m a -> m [a]
fromListT (ListT m) =
  m >>= \case
    NilF -> pure []
    ConsF a xs -> fmap (a :) $ fromListT xs

-- | The join operation of the 'ListT' @m@ monad.
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

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @ListT m@ to @ListT n@.
hoistListT :: Functor n => (forall x. m x -> n x) -> ListT m a -> ListT n a
hoistListT f = ListT . fmap (fmap (hoistListT f)) . f . runListT

-- | Align two 'ListT's, interleaving their effects.
alignListT :: Monad m => ListT m a -> ListT m b -> ListT m (These a b)
alignListT (ListT m) (ListT n) = ListT $ do
  x <- m
  y <- n
  pure $ case (x, y) of
    (NilF, NilF) -> NilF
    (ConsF x' xs, NilF) -> ConsF (This x') (fmap This xs)
    (NilF, ConsF y' ys) -> ConsF (That y') (fmap That ys)
    (ConsF x' xs, ConsF y' ys) -> ConsF (These x' y') (alignListT xs ys)
