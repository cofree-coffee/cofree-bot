module CofreeBot.Bot where

import CofreeBot.Utils
import Control.Arrow qualified as Arrow
import Control.Category qualified as Cat
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Bifunctor
import Data.Functor ((<&>))
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Text qualified as T

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

(\/) :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ either
  ((fmap . fmap . fmap) Left . b1)
  ((fmap . fmap . fmap) Right . b2)

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure $ BotAction (f i) s

impureStatelessBot :: Functor m => (i -> m o) -> Bot m s i o
impureStatelessBot f = Bot $ \i s -> fmap (flip BotAction s) $ f i

-- TODO:
fixedPoint :: Bot m s i o -> s -> i -> m o
fixedPoint = undefined

--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------
-- TODO:

data SessionState s = SessionState { sessions :: Map.Map Int s }

type Sessionized :: KBot -> KBot
type Sessionized bot m s i o = bot m s (SessionInput i) (Int, o)

data SessionInput i = StartSession | InteractWithSession Int i
data SessionOutput o = SessionOutput Int o

parseSessionInput :: Parser i -> Parser (SessionInput i)
parseSessionInput = undefined

runSession :: (Bot m s i o -> Bot m s T.Text T.Text) -> Sessionized Bot m (SessionState s) i o -> Bot m (SessionState s) T.Text T.Text
runSession f bot = Bot $ \i (SessionState s) -> do
  case parseOnly (parseSessionInput undefined) i of
    Left err -> error "Todo"
    Right sessionInput -> error "Todo"

type Serialize m s i o = Bot m s (SessionInput i) (SessionOutput o) -> Sessionized Bot m s (SessionInput T.Text) (SessionOutput T.Text)

-- | Lift a 'Bot' into a 'SessionBot'. 
sessionize :: (Monoid s, Monad m) => Bot m s i o -> Sessionized Bot m (SessionState s) i o
sessionize = error "Todo"
--sessionize (Bot bot) = Bot $ \(k, i) states -> do
--  let state = findOrCreateSession states k
--  BotAction {..} <- bot i state
--  pure $ BotAction { responses = (k, responses), nextState = SessionState $ Map.insert k nextState (sessions states) }

findOrCreateSession :: Monoid s => SessionState s -> Int -> s
findOrCreateSession states k =
  case findSession states k of
    Just s -> s
    Nothing -> mempty

findSession :: SessionState s -> Int -> Maybe s
findSession (SessionState states) k = Map.lookup k states
