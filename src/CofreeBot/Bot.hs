module CofreeBot.Bot where

import Control.Arrow qualified as Arrow
import Control.Category qualified as Cat
import Control.Lens
import CofreeBot.Utils
import Data.Kind
import Data.Profunctor

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
-- Utils
--------------------------------------------------------------------------------

class PointedChoice p where
  pleft :: p a b -> p (x \?/ a) (x \?/ b)
  pright :: p a b -> p (a \?/ x) (b \?/ x)

-- whatever :: (forall p. Choice p => p a b -> p s t) -> (forall p. PointedChoice p => p a b -> p s t)
-- whatever f pab = _

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

infixr /\
(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \i (s, s') -> do
  BotAction{..} <- b1 i s
  BotAction{nextState = nextState', responses = responses'} <- b2 i s'
  pure $ BotAction (responses, responses') (nextState, nextState')

infixr \/
(\/) :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ either
  ((fmap . fmap . fmap) Left . b1)
  ((fmap . fmap . fmap) Right . b2)

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure $ BotAction (f i) s

mapMaybeBot :: (Applicative m, Monoid o) => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
mapMaybeBot f (Bot bot) = Bot $ \i s -> maybe (pure (BotAction mempty s)) (flip bot s) $ f i
