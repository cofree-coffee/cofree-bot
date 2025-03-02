{-# LANGUAGE CPP #-}

-- | Mealy Machine Coalgebras
module Data.Machine.Mealy.Coalgebra
  ( MealyTC (..),
    MealyC,
    fixMealyTC,
    fixMealy,
    scanMealyTC,
    scanMealy,
    processMealyTC,
    processMealy,
    hoistMealyTC,
  )
where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Category (Category)
import Control.Category qualified as Category
import Control.Category.Tensor
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first)
import Data.Machine.Mealy (Mealy, MealyT (..))
import Data.Profunctor (Profunctor, Strong (..))
import Data.Profunctor.Unsafe (Profunctor (..))
import Data.These
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- | A Monadic Mealy Machine consists of:
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A monad @M@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → M S@
--   * A function @observe : S × I → M O@
--
-- In this particular encoding combine the @transition@ and @Observe@
-- functions into @S × I → M (O × S)@. This definition is isomorphic.
newtype MealyTC m s i o = MealyTC {runMealy :: s -> i -> m (o, s)}
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i, MonadIO)
    via StateT s (ReaderT i m)

-- | A Mealy Machine consists of:
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S × I → O@
--
-- In this particular encoding combine the @transition@ and @Observe@
-- functions into @S × I → O × S@. This definition is isomorphic.
type MealyC = MealyTC Identity

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (MealyTC m) where
  combine :: (MealyTC m s i o, MealyTC m t i' o') -> MealyTC m (s, t) (i, i') (o, o')
  combine (MealyTC m1, MealyTC m2) = MealyTC $ \(s, t) (i, i') ->
    liftA2 (curry (\((o, s'), (o', t')) -> ((o, o'), (s', t')))) (m1 s i) (m2 t i')

instance (Functor m) => Trifunctor.Semigroupal (->) (,) Either Either (,) (MealyTC m) where
  combine :: (MealyTC m s i o, MealyTC m t i' o') -> MealyTC m (s, t) (Either i i') (Either o o')
  combine (MealyTC m1, MealyTC m2) = MealyTC $ \(s, t) -> \case
    Left i -> (bimap Left (,t) <$> m1 s i)
    Right i' -> bimap Right (s,) <$> m2 t i'

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) These These (,) (MealyTC m) where
  combine :: (MealyTC m s i o, MealyTC m t i' o') -> MealyTC m (s, t) (These i i') (These o o')
  combine (MealyTC m1, MealyTC m2) = MealyTC $ \(s, t) -> \case
    This i -> bimap This (,t) <$> m1 s i
    That i' -> bimap That (s,) <$> m2 t i'
    These i i' -> do
      liftA2 (curry (\((o, s'), (o', t')) -> (These o o', (s', t')))) (m1 s i) (m2 t i')

instance (Applicative m) => Trifunctor.Unital (->) () () () () (MealyTC m) where
  introduce :: () -> MealyTC m () () ()
  introduce () = MealyTC $ \() () -> pure ((), ())

instance Trifunctor.Unital (->) () Void Void () (MealyTC m) where
  introduce :: () -> MealyTC m () Void Void
  introduce () = MealyTC $ \() -> absurd

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (MealyTC m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () Either Void Either Void (,) () (MealyTC m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () These Void These Void (,) () (MealyTC m)

instance (Functor m) => Profunctor (MealyTC m s) where
  dimap :: (i' -> i) -> (o -> o') -> MealyTC m s i o -> MealyTC m s i' o'
  dimap f g (MealyTC mealy) = MealyTC $ fmap (dimap f (fmap (first g))) mealy

instance (Functor m) => Strong (MealyTC m s) where
  first' :: MealyTC m s i o -> MealyTC m s (i, c) (o, c)
  first' (MealyTC mealy) = MealyTC $ \s (i, c) -> first (,c) <$> mealy s i

instance (Monad m) => Category (MealyTC m s) where
  id :: (Monad m) => MealyTC m s a a
  id = MealyTC $ \s i ->
    pure (i, s)

  (.) :: (Monad m) => MealyTC m s b c -> MealyTC m s a b -> MealyTC m s a c
  MealyTC m2 . MealyTC m1 = MealyTC $ \s a ->
    m1 s a >>= uncurry m2 . swap

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @MealyTC m s i o@ to @MealyTC n s i o@
hoistMealyTC :: (Functor n) => (forall x. m x -> n x) -> MealyTC m s i o -> MealyTC n s i o
hoistMealyTC f (MealyTC b) = MealyTC $ \s i -> f $ b s i

--------------------------------------------------------------------------------

-- | Take the fixpoint of @Mealy s i o@ by recursively constructing an
-- @s -> Mealy' i o@ action adn tupling it with the output observation
-- @o@ from its parent action.
fixMealyTC :: forall m s i o. (Functor m) => MealyTC m s i o -> s -> MealyT m i o
fixMealyTC (MealyTC mealy) = go
  where
    go :: s -> MealyT m i o
    go s = MealyT (fmap (second' go) . mealy s)

-- | Monomorphised 'fixMealyT.
fixMealy :: forall s i o. MealyC s i o -> s -> Mealy i o
fixMealy = fixMealyTC

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Mealy' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMealyTC :: (Monad m) => s -> [i] -> MealyTC m s i o -> m [(o, s)]
scanMealyTC initialState inputs machine =
  case inputs of
    [] -> pure []
    input : xs -> do
      (o, s) <- runMealy machine initialState input
      ys <- scanMealyTC s xs machine
      pure $ (o, s) : ys

-- | Monomorphised 'scanMealyT.
scanMealy :: s -> [i] -> MealyC s i o -> [(o, s)]
scanMealy initialState inputs machine = runIdentity $ scanMealyTC initialState inputs machine

-- | Feed inputs into a 'Mealy' Machine and then observe the final
-- result.
processMealyTC :: (Monad m) => s -> [i] -> MealyTC m s i o -> m o
processMealyTC state' inputs machine =
  case inputs of
    [] -> undefined
    [input] -> fmap fst (runMealy machine state' input)
    input : xs -> do
      (_o, s) <- runMealy machine state' input
      processMealyTC s xs machine

-- | Monomorphised 'processMealyT.
processMealy :: s -> [i] -> MealyC s i o -> o
processMealy state' inputs machine = runIdentity $ processMealyTC state' inputs machine
