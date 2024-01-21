{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.ListT
  ( -- * ListT
    ListT (..),
    ListF (..),

    -- * Operations
    emptyListT,
    consListT,
    singletonListT,
    appendListT,
    joinListT,
    toListT,
    fromListT,
    hoistListT,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (ap)
import Control.Monad.Except (MonadError (..), MonadIO (..), MonadTrans (..))
import Data.Align
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import Data.Functor.Classes
import Data.Functor.Monoidal qualified as Functor
import Data.These (These (..))
import Data.Void (Void)

--------------------------------------------------------------------------------

-- | ListT done right, see https://www.haskell.org/haskellwiki/ListT_done_right_alternative
--
-- NOTE: There are several other encodings available on hackage. This
-- particular version best fit our use case.
newtype ListT m a = ListT
  { runListT :: m (ListF a (ListT m a))
  }

instance (Eq1 m) => Eq1 (ListT m) where
  liftEq f (ListT xs) (ListT ys) = liftEq g xs ys
    where
      g NilF NilF = True
      g (ConsF x xs) (ConsF y ys) = f x y && liftEq f xs ys
      g _ _ = False

deriving instance (Eq (m (ListF a (ListT m a))), Eq1 m, Eq a) => Eq (ListT m a)

deriving instance (Show (m (ListF a (ListT m a))), Show1 m, Show a) => Show (ListT m a)

instance Functor m => Functor (ListT m) where
  fmap :: (a -> b) -> ListT m a -> ListT m b
  fmap f (ListT ma) = ListT $ fmap (bimap f (fmap f)) $ ma

instance Monad m => Applicative (ListT m) where
  pure :: Monad m => a -> ListT m a
  pure = ListT . return . (`ConsF` emptyListT)

  (<*>) :: Monad m => ListT m (a -> b) -> ListT m a -> ListT m b
  (<*>) = ap

instance Monad m => Alternative (ListT m) where
  empty :: ListT m a
  empty = nil

  (<|>) :: ListT m a -> ListT m a -> ListT m a
  xs <|> ys = appendListT xs ys

instance Applicative m => Semialign (ListT m) where
  align :: ListT m a -> ListT m b -> ListT m (These a b)
  align (ListT m) (ListT n) =
    ListT $
      liftA2 (,) m n <&> \case
        (NilF, NilF) -> NilF
        (ConsF x' xs, NilF) -> ConsF (This x') (fmap This xs)
        (NilF, ConsF y' ys) -> ConsF (That y') (fmap That ys)
        (ConsF x' xs, ConsF y' ys) -> ConsF (These x' y') (align xs ys)

instance Applicative m => Align (ListT m) where
  nil :: ListT m a
  nil = ListT $ pure NilF

deriving via Functor.FromApplicative (ListT m) instance (Monad m) => Functor.Semigroupal (->) (,) (,) (ListT m)

deriving via Functor.FromApplicative (ListT m) instance (Monad m) => Functor.Unital (->) () () (ListT m)

deriving via Functor.FromAlternative (ListT m) instance (Monad m) => Functor.Semigroupal (->) Either (,) (ListT m)

deriving via Functor.FromAlternative (ListT m) instance (Monad m) => Functor.Unital (->) Void () (ListT m)

deriving via Functor.FromSemialign (ListT m) instance (Monad m) => Functor.Semigroupal (->) These (,) (ListT m)

instance Monad m => Functor.Monoidal (->) (,) () (,) () (ListT m)

instance Monad m => Functor.Monoidal (->) Either Void (,) () (ListT m)

instance Monad m => Functor.Monoidal (->) These Void (,) () (ListT m)

instance Monad m => Monad (ListT m) where
  (>>=) :: ListT m a -> (a -> ListT m b) -> ListT m b
  ma >>= amb = joinListT $ fmap amb ma

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

data ListF a r = NilF | ConsF a r
  deriving (Show, Eq, Functor)

instance Bifunctor ListF where
  bimap :: (a -> b) -> (c -> d) -> ListF a c -> ListF b d
  bimap f g = \case
    NilF -> NilF
    ConsF a r -> ConsF (f a) (g r)

--------------------------------------------------------------------------------

-- | The empty 'ListT'.
emptyListT :: Applicative m => ListT m a
emptyListT = nil

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

appendListT :: Monad m => ListT m a -> ListT m a -> ListT m a
appendListT (ListT xs) ys = ListT $ do
  xs' <- xs
  runListT $ appendListF xs' ys

appendListF :: Monad m => ListF a (ListT m a) -> ListT m a -> ListT m a
appendListF NilF ys = ys
appendListF (ConsF x xs) ys = ListT $ pure $ ConsF x $ appendListT xs ys

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
