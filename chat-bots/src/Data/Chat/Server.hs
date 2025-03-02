{-# LANGUAGE RankNTypes #-}

-- | The core Chat Server encoding.
module Data.Chat.Server
  ( -- * Env
    Env (..),
    fixEnv,
    hoistEnv,

    -- * Execution
    annihilate,
    loop,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.ListT (ListT, fromListT)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Fix (Fix (..))
import Data.Machine.Mealy (MealyT (..))
import Data.Machine.Moore (MooreT (..))
import Data.Machine.Moore.Coalgebra (MooreTC (..), fixMooreTC, hoistMooreTC)
import Data.Profunctor (Profunctor (..))

--------------------------------------------------------------------------------

-- | The dual to a 'Bot'.
-- Given the state @s@, 'Env' produces an input for a 'Bot' and given
-- the 'Bot's output @o@ will produce an updated state @s@.
--
-- Given a @Bot m s i o@, an @Env m s o i@ and an initial state @s@ we
-- can thus carry on a dialog:
--
-- 1. Initialize the 'Env' with the state @s@ to produce an input @i@
-- for the 'Bot'.
-- 2. Feeding @i@ and @s@ into the 'Bot' to produce a new state @s'@
-- and an output @o@.
-- 3. Use the new state @s'@ to produce the next bot input @i'@ and the
-- prior 'Bot' output @o@ to produce the new state @s'@.
-- 4. Repeat from step 2 with @s'@ and @i'@.
newtype Env m s o i = Env {runEnv :: s -> m (i, [o] -> s)}
  deriving (Functor)

instance (Functor m) => Profunctor (Env m s) where
  dimap f g (Env env) = Env $ fmap (fmap (bimap g (lmap (fmap f)))) env

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Env m s o i@ to @Env n s o i@
hoistEnv :: (Functor n) => (forall x. m x -> n x) -> Env m s o i -> Env n s o i
hoistEnv f = coerce . hoistMooreTC f . MooreTC . runEnv

-- | Generate the fixed point of @Env m s o i@ by recursively
-- construction an @s -> Server m o i@ action and tupling it with
-- the output @i@ from its parent action.
fixEnv :: forall m s o i. (Functor m) => Env m s o i -> s -> MooreT m [o] i
fixEnv = fixMooreTC . MooreTC . runEnv

--------------------------------------------------------------------------------

-- | Collapse a @MooreT m o i@ with a @Mealy m i o@ to create a
-- monadic action @m@.
annihilate :: (Monad m) => MooreT m [o] i -> MealyT (ListT m) i o -> Fix m
annihilate (MooreT server) b@(MealyT mealy) = Fix $ do
  (i, nextServer) <- server
  xs <- fromListT $ mealy i
  let o = fmap fst $ xs
      server' = nextServer o
  pure $
    annihilate server' $ case xs of
      [] -> b
      _ -> snd $ last xs

-- | Recursively unfold fixed point @Fix m@.
loop :: (Monad m) => Fix m -> m x
loop (Fix x) = x >>= loop
