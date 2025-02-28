{-# LANGUAGE CPP #-}

-- | Moore Machines and related machinery.
module Data.Machine.Moore.Coalgebra
  ( MooreM (..),
    Moore,
    MooreM' (..),
    Moore',
    fixMooreM,
    fixMoore,
    scanMooreM,
    scanMoore,
    processMooreM,
    processMoore,
    processMooreM',
    processMoore',
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
newtype MooreM m s i o = MooreM {runMooreM :: s -> m (o, i -> s)}
  deriving (Functor)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (i, i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) ->
      liftA2 (curry $ fmap biapply . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) Either (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (Either i i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> liftA2 (curry $ fmap (\(f, g) -> either ((,t) . f) ((s,) . g)) . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Semigroupal (->) (,) These (,) (,) (MooreM m) where
  combine :: (MooreM m s i o, MooreM m t i' o') -> MooreM m (s, t) (These i i') (o, o')
  combine (MooreM m1, MooreM m2) =
    MooreM $ \(s, t) -> liftA2 (curry $ fmap (\(f, g) -> these ((,t) . f) ((s,) . g) (curry (f *** g))) . Bifunctor.combine @(->) @(,) @(,)) (m1 s) (m2 t)

instance (Applicative m) => Trifunctor.Unital (->) () () () () (MooreM m) where
  introduce :: () -> MooreM m () () ()
  introduce () = MooreM $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Unital (->) () Void () () (MooreM m) where
  introduce :: () -> MooreM m () Void ()
  introduce () = MooreM $ \() -> pure ((), const ())

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (MooreM m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () Either Void (,) () (,) () (MooreM m)

instance (Applicative m) => Trifunctor.Monoidal (->) (,) () These Void (,) () (,) () (MooreM m)

instance (Functor m) => Profunctor (MooreM m s) where
  dimap :: (i' -> i) -> (o -> o') -> MooreM m s i o -> MooreM m s i' o'
  dimap f g (MooreM moore) = MooreM $ fmap (fmap (bimap g (lmap f))) moore

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
type Moore = MooreM Identity

--------------------------------------------------------------------------------

-- | Take the fixpoint of @MooreM s i o@ by recursively constructing an
-- @s -> MooreM' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMooreM :: forall m s o i. (Functor m) => MooreM m s o i -> s -> MooreM' m o i
fixMooreM (MooreM moore) = go
  where
    go :: s -> MooreM' m o i
    go s = MooreM' $ fmap (fmap go) <$> moore s

-- | Take the fixpoint of @Moore s i o@ by recursively constructing an
-- @s -> Moore' i o@ action and tupling it with the output observation
-- @o@ from its parent action.
fixMoore :: forall s o i. Moore s o i -> s -> Moore' o i
fixMoore = fixMooreM

--------------------------------------------------------------------------------

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMooreM :: (Monad m) => s -> [i] -> MooreM m s i o -> m [(o, s)]
scanMooreM state' inputs machine = do
  (o, transition) <- runMooreM machine state'
  case inputs of
    [] -> pure [(o, state')]
    i : xs -> do
      ys <- scanMooreM (transition i) xs machine
      pure $ (o, state') : ys

-- | Feed inputs into a 'Moore' Machine and extract the observation at
-- each state/input in a 'scan' style.
scanMoore :: s -> [i] -> Moore s i o -> [(o, s)]
scanMoore state' inputs machine =
  let (o, transition) = runIdentity $ runMooreM machine state'
   in case inputs of
        [] -> [(o, state')]
        i : xs -> (o, state') : scanMoore (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMooreM :: (Monad m) => s -> [i] -> MooreM m s i o -> m o
processMooreM initialState inputs machine = do
  (o, transition) <- runMooreM machine initialState
  case inputs of
    [] -> pure o
    i : xs -> processMooreM (transition i) xs machine

-- | Feed inputs into a 'Moore' Machine and then observe the final
-- result.
processMoore :: s -> [i] -> Moore s i o -> o
processMoore initialState inputs machine =
  let (o, transition) = runIdentity $ runMooreM machine initialState
   in case inputs of
        [] -> o
        i : xs -> processMoore (transition i) xs machine
