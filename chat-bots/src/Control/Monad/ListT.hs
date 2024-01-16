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
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (liftM2)
import Control.Monad.Except (MonadError (..), MonadIO (..), MonadTrans (..))
import Data.Align
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Monoidal.Specialized (merge')
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.Functor.Monoidal qualified as Functor
import Data.These (These (..))
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- | ListT done right, see https://www.haskell.org/haskellwiki/ListT_done_right_alternative
--
-- NOTE: There are several other encodings available on hackage. This
-- particular version best fit our use case.
newtype ListT m a = ListT
  { runListT :: m (ListF a (ListT m a))
  }

instance Functor m => Functor (ListT m) where
  fmap :: (a -> b) -> ListT m a -> ListT m b
  fmap f (ListT ma) = ListT $ fmap (bimap f (fmap f)) $ ma

instance Monad m => Applicative (ListT m) where
  pure :: a -> ListT m a
  pure a = a <$ Functor.introduce @_ @() ()

  liftA2 :: (a -> b -> c) -> ListT m a -> ListT m b -> ListT m c
  liftA2 f xs ys = uncurry f <$> Functor.combine (xs, ys)

instance Monad m => Functor.Semigroupal (->) (,) (,) (ListT m) where
  combine :: (ListT m x, ListT m x') -> ListT m (x, x')
  combine = uncurry (liftM2 (,))

instance Monad m => Functor.Unital (->) () () (ListT m) where
  introduce :: () -> ListT m ()
  introduce () = ListT $ return (() `ConsF` emptyListT)

instance Monad m => Alternative (ListT m) where
  empty :: ListT m a
  empty = absurd <$> Functor.introduce ()

  (<|>) :: ListT m a -> ListT m a -> ListT m a
  (<|>) xs ys = merge' <$> Functor.combine @_ @Either (xs, ys)

instance Monad m => Functor.Semigroupal (->) Either (,) (ListT m) where
  combine :: (ListT m x, ListT m x') -> ListT m (Either x x')
  combine (ListT xs, ListT ys) =
    ListT $ do
      xs' <- xs
      ys' <- ys
      pure $ case (xs', ys') of
        (NilF, NilF) -> NilF
        (ConsF x' xs'', NilF) -> ConsF (Left x') (Left <$> xs'')
        (NilF, ConsF y' ys'') -> ConsF (Right y') (Right <$> ys'')
        (ConsF x' xs'', ConsF y' ys'') ->
          ConsF (Left x') $ ListT $ pure $ ConsF (Right y') $ Functor.combine (xs'', ys'')

instance Applicative m => Functor.Unital (->) Void () (ListT m) where
  introduce :: () -> ListT m Void
  introduce () = ListT $ pure NilF

instance Monad m => Semialign (ListT m) where
  align :: ListT m a -> ListT m b -> ListT m (These a b)
  align = curry Functor.combine

instance Monad m => Functor.Semigroupal (->) These (,) (ListT m) where
  combine :: (ListT m x, ListT m x') -> ListT m (These x x')
  combine (ListT m, ListT n) = ListT $ do
    x <- m
    y <- n
    pure $ case (x, y) of
      (NilF, NilF) -> NilF
      (ConsF x' xs, NilF) -> ConsF (This x') (fmap This xs)
      (NilF, ConsF y' ys) -> ConsF (That y') (fmap That ys)
      (ConsF x' xs, ConsF y' ys) -> ConsF (These x' y') (Functor.combine (xs, ys))

instance MonadTrans ListT where
  lift :: Monad m => m a -> ListT m a
  lift ma = ListT $ fmap (\a -> ConsF a (ListT $ pure NilF)) ma

instance MonadIO m => MonadIO (ListT m) where
  liftIO :: IO a -> ListT m a
  liftIO io = ListT $ liftIO $ fmap (\a -> ConsF a (ListT $ pure NilF)) io

instance MonadError e m => MonadError e (ListT m) where
  throwError :: e -> ListT m a
  throwError = lift . throwError

  catchError :: ListT m a -> (e -> ListT m a) -> ListT m a
  catchError m f = ListT . deepCatch . runListT $ m
    where
      deepCatch m' = fmap deepCatch' m' `catchError` \e -> runListT (f e)

      deepCatch' = \case
        NilF -> NilF
        ConsF a r -> ConsF a (ListT $ deepCatch $ runListT r)

instance Monad m => Monad (ListT m) where
  return :: a -> ListT m a
  return = pure

  (>>=) :: ListT m a -> (a -> ListT m b) -> ListT m b
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
emptyListT = absurd <$> Functor.introduce ()

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
