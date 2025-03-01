{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | The core Chat Bot encoding.
module Data.Chat.Bot
  ( -- * Bot
    Bot (..),
    KBot,

    -- ** Operations
    invmapBot,
    contramapMaybeBot,
    emptyBot,
    pureStatelessBot,
    hoistBot,
    liftEffect,
    fixBot,
    fixBotPersistent,
    readState,
    saveState,

    -- * Behavior
    Behavior (..),

    -- ** Operations
    batch,
    hoistBehavior,
    liftBehavior,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.ListT (ListF (..), ListT (..), emptyListT, hoistListT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, StateT (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Chat.Utils (readFileMaybe)
#if MIN_VERSION_base(4,18,0)
import Control.Applicative (asum)
#elif MIN_VERSION_base(4,16,1)
import Control.Applicative (asum, liftA2)
#else
import Control.Applicative (liftA2)
import Data.Foldable (asum)
#endif
import Data.Functor ((<&>))
import Data.Kind
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import Data.Profunctor.Traversing (Traversing (..))
import Data.Text qualified as Text
import Data.These (These (..))
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Machine.Mealy.Coalgebra (MealyM (..))

--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

-- $setup
-- >>> import Data.Text (Text)

-- | A 'Bot' receives an input type @i@ and a state @s@ then
-- monadically produces a stream of outputs @o@ and states @s@.
--
-- == Examples
--
-- >>> :{
--   let bot :: Bot m () Text Text
--       bot = Bot $ \() i -> if i == "hello" then pure ("hi!", s) else emptyListT
-- :}
--
-- You can use the 'MonadReader' and 'MonadState' to access a monadic interface:
--
-- >>> :{
--   let bot :: Bot m () Text Text
--       bot = Bot $ do
--         () <- get
--         i <- ask
--         if i == "hello" then pure ("hi!", s) else emptyListT
-- :}
newtype Bot m s i o = Bot {runBot :: s -> i -> ListT m (o, s)}
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i, MonadIO)
    via MealyM (ListT m) s i

deriving via (MealyM (ListT m)) instance (Monad m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (Bot m)

deriving via (MealyM (ListT m)) instance (Functor m) => Trifunctor.Semigroupal (->) (,) Either Either (,) (Bot m)

deriving via (MealyM (ListT m)) instance (Monad m) => Trifunctor.Semigroupal (->) (,) These These (,) (Bot m)

deriving via (MealyM (ListT m)) instance (Monad m) => Trifunctor.Unital (->) () () () () (Bot m)

deriving via (MealyM (ListT m)) instance Trifunctor.Unital (->) () Void Void () (Bot m)

deriving via (MealyM (ListT m)) instance (Monad m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (Bot m)

deriving via (MealyM (ListT m)) instance (Applicative m) => Trifunctor.Monoidal (->) (,) () Either Void Either Void (,) () (Bot m)

deriving via (MealyM (ListT m)) instance (Monad m) => Trifunctor.Monoidal (->) (,) () These Void These Void (,) () (Bot m)

deriving via (MealyM (ListT f) s) instance (Functor f) => Profunctor (Bot f s)

deriving via (MealyM (ListT f) s) instance (Functor f) => Strong (Bot f s)

-- | 'Bot' is an invariant functor on @s@ but our types don't quite
-- fit the @Invariant@ typeclass.
invmapBot :: (Functor m) => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \s i -> (b (g s) i) <&> bimap id f

--------------------------------------------------------------------------------

-- | Lift the 'ListT' Nil value into @Bot m s i o@.
emptyBot :: (Monad m) => Bot m s i o
emptyBot = Bot $ \_ _ -> emptyListT

-- | Construct a 'Bot' which maps from @i@ to @o@ without using its
-- state @s@ or monadic action @m@.
pureStatelessBot :: (Monad m) => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \s i -> pure $ (,) (f i) s

-- | Contramap the input to a bot with the ability to fail and only
-- run the bot on success.
contramapMaybeBot :: (Applicative m) => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
contramapMaybeBot f (Bot bot) = Bot $ \s i -> maybe emptyListT (bot s) (f i)

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Bot m s i o@ to @Bot n s i o@
hoistBot :: (Functor n) => (forall x. m x -> n x) -> Bot m s i o -> Bot n s i o
hoistBot f (Bot b) = Bot $ \s i -> hoistListT f $ b s i

-- | Lift a monadic effect @m o@ into a @Bot m s i o@.
liftEffect :: (Monad m) => m o -> Bot m s i o
liftEffect m = Bot $ \s _ -> ListT $ do
  o <- m
  pure $ ConsF (o, s) (ListT $ pure NilF)

-- | Generate the fixed point of @Bot m s i o@ by recursively
-- construction an @s -> Behavior m i o@ action and tupling it with
-- the output @o@ from its parent action.
fixBot :: forall m s i o. (Functor m) => Bot m s i o -> s -> Behavior m i o
fixBot (Bot b) = go
  where
    go :: s -> Behavior m i o
    go s = Behavior $ \i -> second go <$> b s i

-- TODO: A bi-parser typeclass
type Serializable a = (Read a, Show a)

-- | A variation of 'fixBot' where the bot's state is persisted to disk.
fixBotPersistent :: forall m s i o. (MonadIO m, Serializable s) => FilePath -> Bot m s i o -> s -> IO (Behavior m i o)
fixBotPersistent cachePath (Bot bot) initialState = do
  saveState cachePath initialState
  pure go
  where
    go :: Behavior m i o
    go = Behavior $ \i ->
      liftIO (readState cachePath) >>= \case
        Nothing -> error "ERROR: Failed to read Bot State from disk."
        Just oldState -> do
          (output, newState) <- bot oldState i
          liftIO $ saveState cachePath newState
          pure (output, go)

readState :: (Read s) => FilePath -> IO (Maybe s)
readState cachePath = do
  s <- readFileMaybe $ cachePath </> "state"
  pure $ fmap (read . Text.unpack) s

saveState :: (Show s) => FilePath -> s -> IO ()
saveState cachePath state' = do
  createDirectoryIfMissing True cachePath
  writeFile (cachePath </> "state") (show state')

--------------------------------------------------------------------------------

-- | The fixed point of a 'Bot'.
--
-- Notice that the @s@ parameter has disapeared. This allows us to
-- hide the state threading when interpreting a 'Bot' with some 'Env'.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Behavior m i o = Behavior {runBehavior :: i -> ListT m (o, (Behavior m i o))}

instance (Functor m) => Profunctor (Behavior m) where
  dimap :: (Functor m) => (a -> b) -> (c -> d) -> Behavior m b c -> Behavior m a d
  dimap f g (Behavior b) = Behavior $ dimap f (fmap (bimap g (dimap f g))) b

instance (Monad m) => Bifunctor.Semigroupal (->) (,) (,) (,) (Behavior m) where
  combine :: (Behavior m i o, Behavior m i' o') -> Behavior m (i, i') (o, o')
  combine (Behavior m1, Behavior m2) = Behavior $ \(i, i') -> do
    liftA2 (uncurry (\o m1' (o', m2') -> ((o, o'), Bifunctor.combine (m1', m2')))) (m1 i) (m2 i')

instance (Monad m) => Bifunctor.Unital (->) () () () (Behavior m) where
  introduce :: () -> Behavior m () ()
  introduce () = Behavior $ \() -> pure ((), Bifunctor.introduce ())

instance (Monad m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (Behavior m)

instance (Monad m) => Choice (Behavior m) where
  left' :: (Monad m) => Behavior m a b -> Behavior m (Either a c) (Either b c)
  left' (Behavior b) =
    Behavior $
      either
        (fmap (bimap Left left') . b)
        (pure . (,left' (Behavior b)) . Right)

instance (Functor m) => Strong (Behavior m) where
  first' :: (Functor m) => Behavior m a b -> Behavior m (a, c) (b, c)
  first' (Behavior b) = Behavior $ \(a, c) -> fmap (bimap (,c) first') (b a)

instance (Monad m) => Traversing (Behavior m) where
  -- TODO: write wander instead for efficiency
  traverse' :: (Monad m, Traversable f) => Behavior m a b -> Behavior m (f a) (f b)
  traverse' b = Behavior $ \is ->
    fmap (uncurry (,) . fmap traverse') $
      flip runStateT b $
        traverse
          ( \i -> StateT $ \(Behavior b') ->
              fmap (\(responses, nextState) -> (responses, nextState)) $ b' i
          )
          is

--------------------------------------------------------------------------------

-- | Batch process a list of inputs @i@ with a single 'Behavior',
-- interleaving the effects, and collecting the resulting outputs @o@.
batch :: (Monad m) => Behavior m i o -> Behavior m [i] o
batch (Behavior b) = Behavior $ fmap (fmap batch) . asum . fmap b

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Behavior m s i o@ to @Behavior n s i o@
hoistBehavior :: (Functor n, Functor m) => (forall x. m x -> n x) -> Behavior m i o -> Behavior n i o
hoistBehavior f (Behavior b) = Behavior $ \i -> hoistListT f $ fmap (fmap (hoistBehavior f)) $ b i

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'Behavior'.
liftBehavior :: (Functor (t m), Monad m, MonadTrans t) => Behavior m i o -> Behavior (t m) i o
liftBehavior = hoistBehavior lift
