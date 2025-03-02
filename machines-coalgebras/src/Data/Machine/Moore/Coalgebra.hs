{-# LANGUAGE CPP #-}

-- | Moore Machines and related machinery.
module Data.Machine.Moore.Coalgebra
  ( MooreTC (..),
    Moore,
    hoistMooreTC,
    fixMooreTC,
    fixMoore,
    scanMooreTC,
    scanMoore,
    processMooreTC,
    processMooreC,
  )
where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Arrow
import Control.Monad.Identity (Identity (..))
import Data.Biapplicative
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Bifunctor.Monoidal.Specialized (biapply)
import Data.Machine.Moore
import Data.Profunctor (Profunctor (..))
import Data.These (These (..), these)
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void)

--------------------------------------------------------------------------------

-- | Monadic Moore Machine consists of:
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A monad @M@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S → O@
--
-- In this particular encoding we receive the initial state and
-- produce a tuple of the observation at the initial state and the
-- next state transition function.
newtype MooreTC m s i o = MooreTC {runMooreTC :: s -> m (o, i -> s)}
  deriving (Functor)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (MooreTC m) where
  combine :: (MooreTC m s i o, MooreTC m t i' o') -> MooreTC m (s, t) (i, i') (o, o')
  combine (MooreTC m1, MooreTC m2) =
    MooreTC $ \(s, t) ->
      liftA2 (curry $ fmap biapply . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) Either (,) (,) (MooreTC m) where
  combine :: (MooreTC m s i o, MooreTC m t i' o') -> MooreTC m (s, t) (Either i i') (o, o')
  combine (MooreTC m1, MooreTC m2) =
    MooreTC $ \(s, t) -> liftA2 (curry $ fmap (\(f, g) -> either ((,t) . f) ((s,) . g)) . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) These (,) (,) (MooreTC m) where
  combine :: (MooreTC m s i o, MooreTC m t i' o') -> MooreTC m (s, t) (These i i') (o, o')
  combine (MooreTC m1, MooreTC m2) =
    MooreTC $ \(s, t) -> liftA2 (curry $ fmap (\(f, g) -> these ((,t) . f) ((s,) . g) (curry (f *** g))) . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Unital (->) () () () () (MooreTC m) where
  introduce :: () -> MooreTC m () () ()
  introduce () = MooreTC $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Unital (->) () Void () () (MooreTC m) where
  introduce :: () -> MooreTC m () Void ()
  introduce () = MooreTC $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (MooreTC m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () Either Void (,) () (,) () (MooreTC m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () These Void (,) () (,) () (MooreTC m)

instance (Functor m) => Profunctor (MooreTC m s) where
  dimap :: (i' -> i) -> (o -> o') -> MooreTC m s i o -> MooreTC m s i' o'
  dimap f g (MooreTC moore) = MooreTC $ fmap (fmap (bimap g (lmap f))) moore

-- | Moore Machine
--
--   * A finite set of states @S@
--   * An initial state @s : S@
--   * A finite set called the input @I@
--   * A finite set called the output @O@
--   * A function @transistion : S × I → S@
--   * A function @observe : S → O@
--
-- In this particular encoding we receive the initial state and
-- produce a tuple of the observation at the initial state and the
-- next state transition function.
type MooreC = MooreTC Identity

--------------------------------------------------------------------------------

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @MooreTC m s o i@ to @MooreTC n s o i@
hoistMooreTC :: (Functor n) => (forall x. m x -> n x) -> MooreTC m s o i -> MooreTC n s o i
hoistMooreTC f (MooreTC moore) = MooreTC $ \s -> f $ moore s

--------------------------------------------------------------------------------

-- | Take the fixpoint of @MooreTC s i o@ by recursively constructing an
-- @s -> MooreTC' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMooreTC :: forall m s o i. (Functor m) => MooreTC m s o i -> s -> MooreT m o i
fixMooreTC (MooreTC moore) = go
  where
    go :: s -> MooreT m o i
    go s = MooreT $ fmap (fmap go) <$> moore s

-- | Take the fixpoint of @Moore s i o@ by recursively constructing an
-- @s -> Moore' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMoore :: forall s o i. MooreC s o i -> s -> Moore o i
fixMoore = fixMooreTC

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMooreTC :: (Monad m) => s -> [i] -> MooreTC m s i o -> m [(o, s)]
scanMooreTC state' inputs machine = do
  (o, transition) <- runMooreTC machine state'
  case inputs of
    [] -> pure [(o, state')]
    i : xs -> do
      ys <- scanMooreTC (transition i) xs machine
      pure $ (o, state') : ys

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMoore :: s -> [i] -> MooreC s i o -> [(o, s)]
scanMoore state' inputs machine =
  let (o, transition) = runIdentity $ runMooreTC machine state'
   in case inputs of
        [] -> [(o, state')]
        i : xs -> (o, state') : scanMoore (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMooreTC :: (Monad m) => s -> [i] -> MooreTC m s i o -> m o
processMooreTC initialState inputs machine = do
  (o, transition) <- runMooreTC machine initialState
  case inputs of
    [] -> pure o
    i : xs -> processMooreTC (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMooreC :: s -> [i] -> MooreC s i o -> o
processMooreC initialState inputs machine =
  let (o, transition) = runIdentity $ runMooreTC machine initialState
   in case inputs of
        [] -> o
        i : xs -> processMooreC (transition i) xs machine
