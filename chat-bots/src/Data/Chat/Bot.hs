{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

    -- ** Operations
    batch,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.ListT (ListF (..), ListT (..), emptyListT, hoistListT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Data.Bifunctor (Bifunctor (..))
import Data.Chat.Utils (readFileMaybe)
#if MIN_VERSION_base(4,18,0)
import Control.Applicative (asum)
#elif MIN_VERSION_base(4,16,1)
import Control.Applicative (asum, liftA2)
#else
import Control.Applicative (liftA2)
import Data.Foldable (asum)
#endif
import Control.Category (Category (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<&>))
import Data.Kind
import Data.Machine.Mealy (MealyT (..))
import Data.Machine.Mealy.Coalgebra (MealyTC (..), fixMealyTC)
import Data.Profunctor (Profunctor (..), Strong (..))
import Data.Text.Encoding (encodeUtf8)
import Data.These (These (..))
import Data.Trifunctor.Monoidal qualified as Trifunctor
import Data.Void (Void)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Prelude hiding (id, (.))

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
    via MealyTC (ListT m) s i

deriving via (MealyTC (ListT m)) instance (Monad m) => Trifunctor.Semigroupal (->) (,) (,) (,) (,) (Bot m)

deriving via (MealyTC (ListT m)) instance (Functor m) => Trifunctor.Semigroupal (->) (,) Either Either (,) (Bot m)

deriving via (MealyTC (ListT m)) instance (Monad m) => Trifunctor.Semigroupal (->) (,) These These (,) (Bot m)

deriving via (MealyTC (ListT m)) instance (Monad m) => Trifunctor.Unital (->) () () () () (Bot m)

deriving via (MealyTC (ListT m)) instance Trifunctor.Unital (->) () Void Void () (Bot m)

deriving via (MealyTC (ListT m)) instance (Monad m) => Trifunctor.Monoidal (->) (,) () (,) () (,) () (,) () (Bot m)

deriving via (MealyTC (ListT m)) instance (Applicative m) => Trifunctor.Monoidal (->) (,) () Either Void Either Void (,) () (Bot m)

deriving via (MealyTC (ListT m)) instance (Monad m) => Trifunctor.Monoidal (->) (,) () These Void These Void (,) () (Bot m)

deriving via (MealyTC (ListT f) s) instance (Functor f) => Profunctor (Bot f s)

deriving via (MealyTC (ListT f) s) instance (Functor f) => Strong (Bot f s)

deriving via (MealyTC (ListT m) s) instance (Monad m) => Category (Bot m s)

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

-- | Generate the fixed point of @Bot m s i o@ by recursively construction an @s
-- -> MealyM' (ListT m) i o@ action and tupling it with the output @o@ from its
-- parent action.
fixBot :: forall m s i o. (Functor m) => Bot m s i o -> s -> MealyT (ListT m) i o
fixBot = fixMealyTC . MealyTC . runBot

-- TODO: A bi-parser typeclass
type Serializable a = (FromJSON a, ToJSON a)

fixBotPersistent :: forall m s i o. (MonadIO m, Serializable s) => FilePath -> Bot m s i o -> s -> IO (MealyT (ListT m) i o)
fixBotPersistent cachePath (Bot bot) initialState = do
  saveState cachePath initialState
  pure go
  where
    go :: MealyT (ListT m) i o
    go = MealyT $ \i ->
      liftIO (readState cachePath) >>= \case
        Nothing -> error "ERROR: Failed to read Bot State from disk."
        Just oldState -> do
          (output, newState) <- bot oldState i
          liftIO $ saveState cachePath newState
          pure (output, go)

readState :: (FromJSON s) => FilePath -> IO (Maybe s)
readState cachePath = do
  s <- readFileMaybe $ cachePath </> "state"
  pure $ s >>= Aeson.decode . BL.fromStrict . encodeUtf8

saveState :: (ToJSON s) => FilePath -> s -> IO ()
saveState cachePath state' = do
  createDirectoryIfMissing True cachePath
  BL.writeFile (cachePath </> "state") (Aeson.encode state')

--------------------------------------------------------------------------------

-- | Batch process a list of inputs @i@ with a single 'Behavior',
-- interleaving the effects, and collecting the resulting outputs @o@.
batch :: (Monad m) => MealyT (ListT m) i o -> MealyT (ListT m) [i] o
batch (MealyT b) = MealyT $ fmap (fmap batch) . asum . fmap b
