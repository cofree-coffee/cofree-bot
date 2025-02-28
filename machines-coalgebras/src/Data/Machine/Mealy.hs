{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Machine.Mealy where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Category (Category (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Functor.Identity (Identity)
import Data.Profunctor (Choice, Profunctor (..), Strong (..))
import Data.Profunctor.Choice (Choice (..))
import Prelude hiding (id, (.))

--------------------------------------------------------------------------------

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype MealyM' m i o = MealyM' {runMealyM' :: i -> m (o, MealyM' m i o)}
  deriving stock (Functor)

-- | The fixed point of a 'Mealy' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Mealy' = MealyM' Identity

instance (Applicative m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MealyM' m) where
  combine :: (MealyM' m i o, MealyM' m i' o') -> MealyM' m (i, i') (o, o')
  combine (MealyM' m1, MealyM' m2) = MealyM' $ \(i, i') -> do
    liftA2 (uncurry (\o m1' (o', m2') -> ((o, o'), Bifunctor.combine (m1', m2')))) (m1 i) (m2 i')

instance (Applicative m) => Bifunctor.Unital (->) () () () (MealyM' m) where
  introduce :: () -> MealyM' m () ()
  introduce () = MealyM' $ \() -> pure ((), Bifunctor.introduce ())

instance (Applicative m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MealyM' m)

instance (Functor m) => Profunctor (MealyM' m) where
  dimap :: (i' -> i) -> (o -> o') -> MealyM' m i o -> MealyM' m i' o'
  dimap f g (MealyM' mealy) = MealyM' $ dimap f (fmap (bimap g (dimap f g))) mealy

instance (Functor m) => Strong (MealyM' m) where
  first' :: MealyM' m i o -> MealyM' m (i, x) (o, x)
  first' (MealyM' mealy) = MealyM' $ \(i, x) -> bimap (,x) first' <$> mealy i

instance (Applicative m) => Choice (MealyM' m) where
  left' :: MealyM' m i o -> MealyM' m (Either i x) (Either o x)
  left' (MealyM' mealy) = MealyM' $ either (fmap (bimap Left left') . mealy) (pure . (,left' (MealyM' mealy)) . Right)

instance (Monad m) => Category (MealyM' m) where
  id :: (Monad m) => MealyM' m a a
  id = MealyM' $ \a ->
    pure (a, id)

  (.) :: (Monad m) => MealyM' m b c -> MealyM' m a b -> MealyM' m a c
  MealyM' m2 . MealyM' m1 = MealyM' $ \a -> do
    (b, m1') <- m1 a
    (c, m2') <- m2 b
    pure (c, m2' . m1')

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @MealyM' m s i o@ to @MealyM' n s i o@
hoistMealyM' :: (Functor n, Functor m) => (forall x. m x -> n x) -> MealyM' m i o -> MealyM' n i o
hoistMealyM' f (MealyM' mealy) = MealyM' $ \i -> f (fmap (hoistMealyM' f) <$> mealy i)

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'MealyM''.
liftMealyM' :: (Functor (t m), Monad m, MonadTrans t) => MealyM' m i o -> MealyM' (t m) i o
liftMealyM' = hoistMealyM' lift
