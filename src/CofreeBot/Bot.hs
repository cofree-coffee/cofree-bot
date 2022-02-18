{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE RankNTypes #-}
module CofreeBot.Bot where

import           CofreeBot.Utils
import qualified Control.Arrow                 as Arrow
import qualified Control.Category              as Cat
import           Control.Monad.State            ( StateT(StateT)
                                                , runStateT
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Functor                   ( (<&>) )
import           Data.Kind
import           Data.Profunctor
import           Data.Profunctor.Traversing

--------------------------------------------------------------------------------
-- Kinds
--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

--------------------------------------------------------------------------------
-- Bot Action
--------------------------------------------------------------------------------

data BotAction s o = BotAction
  { responses :: o
  , nextState :: s
  }
  deriving Functor

instance (Semigroup s, Semigroup o) => Semigroup (BotAction s o) where
  BotAction o s <> BotAction o' s' =
    BotAction { responses = o <> o', nextState = s <> s' }

instance (Monoid s, Monoid o) => Monoid (BotAction s o) where
  mempty = BotAction { responses = mempty, nextState = mempty }

instance Bifunctor BotAction where
  bimap f g (BotAction a b) = BotAction (g a) (f b)

--------------------------------------------------------------------------------
-- Bot
--------------------------------------------------------------------------------

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot
newtype Bot m s i o = Bot { runBot :: i -> s -> m (BotAction s o) }

instance Monad m => Cat.Category (Bot m s) where
  id = Bot $ \a s -> pure $ BotAction a s

  Bot f . Bot g = Bot $ \a s -> do
    BotAction b s' <- g a s
    f b s'

instance Monad m => Arrow.Arrow (Bot m s) where
  arr f = rmap f Cat.id
  first = first'

instance Functor f => Profunctor (Bot f s) where
  dimap f g (Bot bot) = Bot $ \a -> fmap (fmap g) . bot (f a)

instance Functor f => Strong (Bot f s) where
  first' (Bot bot) = Bot $ \(a, c) -> fmap (fmap (, c)) . bot a

instance Applicative f => Choice (Bot f s) where
  left' (Bot bot) = Bot $ either ((fmap . fmap . fmap) Left . bot)
                                 (\c s -> pure $ BotAction (Right c) s)

--------------------------------------------------------------------------------
-- Behavior
--------------------------------------------------------------------------------

newtype Behavior m i o = Behavior { runBehavior :: i -> m (BotAction (Behavior m i o) o) }

instance Functor m => Functor (Behavior m i)
  where
  fmap f (Behavior b) = Behavior $ fmap (fmap $ bimap (fmap f) f) b

instance Functor m => Profunctor (Behavior m)
  where
  dimap f g (Behavior b) = Behavior $ dimap f (fmap $ bimap (dimap f g) g) b

instance Applicative m => Choice (Behavior m)
  where
  left' (Behavior b) = Behavior $ either
    (fmap (bimap left' Left) . b)
    (pure . flip BotAction (left' (Behavior b)) . Right)

instance Functor m => Strong (Behavior m)
  where
  first' (Behavior b) = Behavior $ \(a, c) -> fmap (bimap first' (, c)) $ b a

instance Monad m => Traversing (Behavior m)
  where
  -- TODO: write wander instead for efficiency

  traverse' b = Behavior $ \is ->
    fmap (uncurry BotAction . fmap traverse') $ flip runStateT b $ traverse
      (\i -> StateT $ \(Behavior b') ->
        fmap
            (\case
              BotAction {..} -> (responses, nextState)
            )
          $ b' i
      )
      is

--------------------------------------------------------------------------------
-- Fix
--------------------------------------------------------------------------------

newtype Fix f = Fix { runFix :: f (Fix f) }

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | 'Bot' is an invariant functor on 's' but we cannot write an instance in Haskell.
invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \i s -> b i (g s) <&> first f

nudge
  :: Applicative m
  => Bot m s i o \/ Bot m s i' o'
  -> Bot m s (i \/ i') (o \?/ o')
nudge = either
  (\(Bot b) -> Bot $ either ((fmap . fmap . fmap . fmap) (Just . Left) b)
                            (const $ \s -> pure $ BotAction Nothing s)
  )
  (\(Bot b) -> Bot $ either (const $ \s -> pure $ BotAction Nothing s)
                            ((fmap . fmap . fmap . fmap) (Just . Right) b)
  )

nudgeLeft :: Applicative m => Bot m s i o -> Bot m s (i \/ i') (o \?/ o')
nudgeLeft = nudge . Left

nudgeRight :: Applicative m => Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudgeRight = nudge . Right

infixr /\
(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \i (s, s') -> do
  BotAction {..} <- b1 i s
  BotAction { nextState = nextState', responses = responses' } <- b2 i s'
  pure $ BotAction (responses, responses') (nextState, nextState')

infixr \/
(\/)
  :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot
  $ either ((fmap . fmap . fmap) Left . b1) ((fmap . fmap . fmap) Right . b2)

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure $ BotAction (f i) s

mapMaybeBot
  :: (Applicative m, Monoid o) => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
mapMaybeBot f (Bot bot) =
  Bot $ \i s -> maybe (pure (BotAction mempty s)) (`bot` s) $ f i

emptyBot :: (Monoid o, Applicative m) => Bot m s i o
emptyBot = pureStatelessBot $ const mempty

hoistBot :: (forall x . m x -> n x) -> Bot m s i o -> Bot n s i o
hoistBot f (Bot b) = Bot $ fmap (fmap f) b

fixBot :: Functor m => Bot m s i o -> s -> Behavior m i o
fixBot (Bot b) = go where go s = Behavior $ \i -> first go <$> b i s
