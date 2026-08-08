{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
-- newtype MealyT m i o = MealyT {runMealyT :: i -> m (o, MealyT m i o)}
module Data.Machine.Mealy
  ( MealyT (..),
    Mealy,
    hoistMealyT,
    liftMealyT,
  )
where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Category (Category (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Machine.MealyT (MealyT (..))
import Data.Profunctor (Strong (..))
import Data.Profunctor.Choice (Choice (..), Cochoice (..))
import Data.Profunctor.Closed (Closed (..))
import Data.Profunctor.Rep (Corepresentable (..), unfirstCorep)
import Data.Profunctor.Sieve (Cosieve (..))
import Data.Profunctor.Strong (Costrong (..))
import Prelude hiding (id, (.))

--------------------------------------------------------------------------------

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Mealy = MealyT Identity

instance (Applicative m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MealyT m) where
  combine :: (MealyT m i o, MealyT m i' o') -> MealyT m (i, i') (o, o')
  combine (MealyT m1, MealyT m2) = MealyT $ \(i, i') -> do
    liftA2 (uncurry (\o m1' (o', m2') -> ((o, o'), Bifunctor.combine (m1', m2')))) (m1 i) (m2 i')

instance (Applicative m) => Bifunctor.Unital (->) () () () (MealyT m) where
  introduce :: () -> MealyT m () ()
  introduce () = MealyT $ \() -> pure ((), Bifunctor.introduce ())

instance (Applicative m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MealyT m)

instance (Functor m) => Strong (MealyT m) where
  first' :: MealyT m i o -> MealyT m (i, x) (o, x)
  first' (MealyT mealy) = MealyT $ \(i, x) -> bimap (,x) first' <$> mealy i

instance (Applicative m) => Choice (MealyT m) where
  left' :: MealyT m i o -> MealyT m (Either i x) (Either o x)
  left' (MealyT mealy) = MealyT $ either (fmap (bimap Left left') . mealy) (pure . (,left' (MealyT mealy)) . Right)

instance (Monad m) => Cochoice (MealyT m) where
  unleft :: MealyT m (Either a d) (Either b d) -> MealyT m a b
  unleft m = MealyT $ \a -> go m (Left a)
    where
      go :: MealyT m (Either a d) (Either b d) -> Either a d -> m (b, MealyT m a b)
      go (MealyT mealy) x = do
        mealy x >>= \case
          (Left b, m) -> pure (b, unleft m)
          (Right d, m) -> go m (Right d)

instance Cosieve (MealyT Identity) NonEmpty where
  cosieve :: MealyT Identity a b -> NonEmpty a -> b
  cosieve (MealyT m) (x :| xs) = runIdentity $ do
    (b, MealyT m) <- m x
    case xs of
      [] -> pure b
      x : xs -> pure $ cosieve (MealyT m) (x :| xs)

instance Corepresentable (MealyT Identity) where
  type Corep (MealyT Identity) = NonEmpty

  cotabulate :: (Corep (MealyT Identity) d -> c) -> MealyT Identity d c
  cotabulate f = MealyT $ \a -> Identity $ go [a] f
    where
      go as f = (f (NonEmpty.fromList $ reverse as), MealyT $ \a -> Identity $ go (a : as) f)

instance Costrong (MealyT Identity) where
  unfirst :: MealyT Identity (a, d) (b, d) -> MealyT Identity a b
  unfirst = unfirstCorep

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @MealyT m s i o@ to @MealyT n s i o@
hoistMealyT :: (Functor n, Functor m) => (forall x. m x -> n x) -> MealyT m i o -> MealyT n i o
hoistMealyT f (MealyT mealy) = MealyT $ \i -> f (fmap (hoistMealyT f) <$> mealy i)

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'MealyT'.
liftMealyT :: (Functor (t m), Monad m, MonadTrans t) => MealyT m i o -> MealyT (t m) i o
liftMealyT = hoistMealyT lift
