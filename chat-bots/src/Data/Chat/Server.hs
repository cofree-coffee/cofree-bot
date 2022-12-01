{-# LANGUAGE RankNTypes #-}

module Data.Chat.Server
  ( -- * Env
    Env (..),
    fixEnv,
    hoistEnv,

    -- * Server
    Server (..),
    annihilate,
    loop,
    hoistServer,
    liftServer,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.ListT (fromListT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Bifunctor (bimap)
import Data.Chat.Bot
import Data.Fix (Fix (..))
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

instance Functor m => Profunctor (Env m s) where
  dimap f g (Env env) = Env $ fmap (fmap (bimap g (lmap (fmap f)))) env

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Env m s o i@ to @Env n s o i@
hoistEnv :: Functor n => (forall x. m x -> n x) -> Env m s o i -> Env n s o i
hoistEnv f (Env env) = Env $ \s -> f $ env s

--------------------------------------------------------------------------------

-- | The fixed point of an 'Env'. Like in 'Behavior' we have factored
-- out the @s@ parameter to hide the state threading.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Server m o i = Server {runServer :: m (i, [o] -> Server m o i)}
  deriving (Functor)

instance Functor m => Profunctor (Server m) where
  dimap f g (Server serve) =
    Server $ fmap (bimap g (dimap (fmap f) (dimap f g))) serve

-- | Generate the fixed point of @Env m s o i@ by recursively
-- construction an @s -> Server m o i@ action and tupling it with
-- the output @i@ from its parent action.
fixEnv :: forall m s o i. Functor m => Env m s o i -> s -> Server m o i
fixEnv (Env b) = go
  where
    go :: s -> Server m o i
    go s = Server $ fmap (fmap (fmap go)) $ b s

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Env m s o i@ to @Env n s o i@
hoistServer :: Functor n => (forall x. m x -> n x) -> Server m o i -> Server n o i
hoistServer f (Server server) = Server $ fmap (fmap (fmap (hoistServer f))) $ f server

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'Server'.
liftServer :: (Functor (t m), Monad m, MonadTrans t) => Server m o i -> Server (t m) o i
liftServer = hoistServer lift

--------------------------------------------------------------------------------

-- | Collapse a @Server m o i@ with a @Bahavior m i o@ to create a
-- monadic action @m@.
annihilate :: Monad m => Server m o i -> Behavior m i o -> Fix m
annihilate (Server server) b@(Behavior botBehavior) = Fix $ do
  (i, nextServer) <- server
  xs <- fromListT $ botBehavior i
  let o = fmap fst $ xs
      server' = nextServer o
  pure $
    annihilate server' $ case xs of
      [] -> b
      _ -> snd $ last xs

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop
