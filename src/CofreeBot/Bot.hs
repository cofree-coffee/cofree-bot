{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module CofreeBot.Bot where

import Control.Arrow qualified as Arrow
import Control.Category qualified as Cat
import Control.Lens
import CofreeBot.Utils
import Data.Kind
import Data.Profunctor
import Data.Text qualified as T
import Control.Monad.State

data BotAction s o = BotAction { responses :: o, nextState :: s }
  deriving Functor

instance (Semigroup s, Semigroup o) => Semigroup (BotAction s o) where
   (BotAction o s) <> (BotAction o' s') =
       BotAction {responses = o <> o', nextState = s <> s'} 

instance (Monoid s, Monoid o) => Monoid (BotAction s o) where
  mempty = BotAction {responses = mempty, nextState = mempty}

instance Bifunctor BotAction where
  bimap f g (BotAction a b) = BotAction (g a) (f b)

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

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
  arr f = rmap f (Cat.id)
  first = first'

instance Functor f => Profunctor (Bot f s) where
  dimap f g (Bot bot) = Bot $ \a -> fmap (fmap g) . bot (f a)

instance Functor f => Strong (Bot f s) where
  first' (Bot bot) = Bot $ \(a, c) -> fmap (fmap (,c)) . bot a

instance Applicative f => Choice (Bot f s) where
  left' (Bot bot) = Bot $ either
    ((fmap . fmap . fmap) Left . bot)
    (\c s -> pure $ BotAction (Right c) s)

-- | 'Bot' is an invariant functor on 's' but we cannot write an instance in Haskell.
invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \i s -> (b i (g s)) <&> bimap f id

--------------------------------------------------------------------------------
-- Testing Harness
--------------------------------------------------------------------------------

class Monad m => MonadHarness i o m | m -> i o where
  receive :: m i
  send :: o -> m ()

data Harness i o x
  = Receive (i -> Harness i o x)
  | Send o (Harness i o x)
  | Done x
  deriving Functor

instance Applicative (Harness i o) where
  pure = return
  (<*>) = ap

instance Monad (Harness i o) where
  return a = Done a
  Receive i >>= f = Receive $ (>>= f) . i
  Send o h >>= f = Send o (h >>= f)
  Done x >>= f = f x

instance MonadHarness i o (Harness i o) where
  receive = Receive $ \i -> Done i
  send o = Send o (Done ())
    
runHarness :: MonadHarness i o m => Bot m s i o -> s -> m ()
runHarness (Bot bot) = go
  where
    go s = do
      i <- receive
      BotAction{..} <- bot i s
      send responses
      go nextState

data HarnessF i o x = ReceiveF (i -> x) | SendF (o, x)

data Coharness i o x = Coharness
  { inputResult :: (i, Coharness i o x)
  , outputResult :: o -> Coharness i o x
  , doneResult :: x
  }

data CoharnessF i o x = CoharnessF
  { inputResultF :: (i, x)
  , outputResultF :: o -> x
  }

ourOtherFunc :: Coharness i o (Harness i o x) -> x
ourOtherFunc Coharness {..}
  = case doneResult of
      Receive f ->
        case inputResult of
          (i, co) -> _ $ f i
      Send o har -> _
      Done x -> x

ourFunc :: Harness i o x -> ([o] -> i) -> [o] -> (x, [o])
ourFunc (Receive r) f os = ourFunc (r $ f os) f os
ourFunc (Send o h) f os = ourFunc h f (o:os) 
ourFunc (Done x) _ os = (x, os)

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

class PointedChoice p where
  pleft :: p a b -> p (x \?/ a) (x \?/ b)
  pright :: p a b -> p (a \?/ x) (b \?/ x)

nudge :: Applicative m => Bot m s i o \/ Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudge = either
  (\(Bot b) ->
    Bot $ either
      ((fmap . fmap . fmap . fmap) (Just . Left) $ b)
      (const $ \s -> pure $ BotAction Nothing s))
  (\(Bot b) ->
    Bot $ either
      (const $ \s -> pure $ BotAction Nothing s)
      ((fmap . fmap . fmap . fmap) (Just . Right) $ b))

nudgeLeft :: Applicative m => Bot m s i o -> Bot m s (i \/ i') (o \?/ o')
nudgeLeft = nudge . Left

nudgeRight :: Applicative m => Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudgeRight = nudge . Right

(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \i (s, s') -> do
  BotAction{..} <- b1 i s
  BotAction{nextState = nextState', responses = responses'} <- b2 i s'
  pure $ BotAction (responses, responses') (nextState, nextState')

(\/) :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ either
  ((fmap . fmap . fmap) Left . b1)
  ((fmap . fmap . fmap) Right . b2)

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure $ BotAction (f i) s

mapMaybeBot :: (Applicative m, Monoid o) => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
mapMaybeBot f (Bot bot) = Bot $ \i s -> maybe (pure (BotAction mempty s)) (flip bot s) $ f i
