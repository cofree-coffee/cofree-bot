{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE RankNTypes #-}
module CofreeBot.Bot where

import           CofreeBot.Utils
import qualified Control.Arrow                 as Arrow
import qualified Control.Category              as Cat
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Lens (ifolded, view, (^.), _Just)
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Data.Profunctor
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Network.Matrix.Client
import           Network.Matrix.Client.Lens
import           System.Directory               ( createDirectoryIfMissing )
import           System.IO
import           System.IO.Error                ( isDoesNotExistError )
import           System.Random
import Data.Bifunctor (Bifunctor(..))
import Control.Monad.State (runStateT, StateT (StateT))
import Data.Profunctor.Traversing
import Data.Functor ((<&>))

--------------------------------------------------------------------------------
-- Kinds
--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Fix f = Fix { runFix :: f (Fix f) }

data BotAction s o = BotAction
  { responses :: o
  , nextState :: s
  }
  deriving Functor

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot
newtype Bot m s i o = Bot { runBot :: i -> s -> m (BotAction s o) }

newtype Behavior m i o = Behavior { runBehavior :: i -> m (BotAction (Behavior m i o) o) }

instance Functor m => Functor (Behavior m i)
  where
  fmap f (Behavior b) = Behavior $ fmap (fmap $ bimap (fmap f) f) b

instance Functor m => Profunctor (Behavior m)
  where
  dimap f g (Behavior b) = Behavior $ dimap f (fmap $ bimap (dimap f g) g) b

instance Applicative m => Choice (Behavior m)
  where
  left' (Behavior b) = Behavior $ either (fmap (bimap left' Left) . b) (pure . flip BotAction (left' (Behavior b)) . Right)

instance Functor m => Strong (Behavior m)
  where
  first' (Behavior b) = Behavior $ \(a, c) -> fmap (bimap first' (, c)) $ b a

instance Monad m => Traversing (Behavior m)
  where
  -- TODO: write wander instead for efficiency

  traverse' b = Behavior $ \is ->
    fmap (uncurry BotAction . fmap traverse')
    $ flip runStateT b
    $ traverse (\i -> StateT $ \(Behavior b') -> fmap (\case { BotAction {..} -> (responses, nextState) }) $ b' i) is

fixBot :: Functor m => Bot m s i o -> s -> Behavior m i o
fixBot (Bot b) = go
  where
  go s = Behavior $ \i -> first go <$> b i s

type Env :: KBot
newtype Env m s o i = Env { runEnv :: s -> (i, o -> m s) }

newtype Server m o i = Server { runServer :: (i, o -> m (Server m o i)) }

fixEnv :: Functor m => Env m s o i -> s -> Server m o i
fixEnv (Env b) = go
  where
  go s = Server $ fmap (fmap (fmap go)) $ b s

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance (Semigroup s, Semigroup o) => Semigroup (BotAction s o) where
  BotAction o s <> BotAction o' s' =
    BotAction { responses = o <> o', nextState = s <> s' }

instance (Monoid s, Monoid o) => Monoid (BotAction s o) where
  mempty = BotAction { responses = mempty, nextState = mempty }

instance Bifunctor BotAction where
  bimap f g (BotAction a b) = BotAction (g a) (f b)

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

hoistBot :: (forall x. m x -> n x) -> Bot m s i o -> Bot n s i o
hoistBot f (Bot b) = Bot $ fmap (fmap f) b

--------------------------------------------------------------------------------
-- Matrix Bot
--------------------------------------------------------------------------------

type MatrixBot m s = Bot m s (RoomID, Event) [(RoomID, Event)]

liftMatrixIO :: (MonadIO m, MonadError MatrixError m) => MatrixIO x -> m x
liftMatrixIO m = liftEither =<< liftIO m

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path = fmap Just (T.readFile path)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

matrix :: (MonadError MatrixError m, MonadIO m) => ClientSession -> FilePath -> m (Server m [(RoomID, Event)] [(RoomID, Event)])
matrix session cache = do
  -- Setup cache
  liftIO $ createDirectoryIfMissing True cache
  since <- liftIO $ readFileMaybe $ cache <> "/since_file"

  -- Log in
  userId   <- liftMatrixIO $ getTokenOwner session
  filterId <- liftMatrixIO $ createFilter session userId messageFilter

  -- Start looping
  go filterId since

  where
    go filterId since = do
      -- Get conversation events
      syncResult <- liftMatrixIO $ sync session (Just filterId) since (Just Online) (Just 1000)

      -- Unpack sync result
      let newSince :: T.Text
          newSince = syncResult ^. _srNextBatch

          roomsMap :: Map.Map T.Text JoinedRoomSync
          roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded

          roomEvents :: Map.Map T.Text [RoomEvent]
          roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

          events :: [(RoomID, Event)]
          events = Map.foldMapWithKey
            (\rid es -> fmap ((RoomID rid, ) . view _reContent) es)
            roomEvents

      return $ Server $ (events,) $ \outputs -> do
        -- Send the bot's responses
        gen <- newStdGen
        let txnIds = TxnID . T.pack . show <$> randoms @Int gen
        liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

        -- Update since file
        liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)

        -- Do it again
        go filterId (Just newSince)

simplifyMatrixBot :: Monad m => MatrixBot m s -> TextBot m s
simplifyMatrixBot (Bot bot) = Bot $ \i s -> do
  BotAction {..} <- bot (RoomID mempty, mkMsg i) s
  pure $ BotAction (fmap (viewBody . snd) responses) nextState

liftSimpleBot :: Functor m => TextBot m s -> MatrixBot m s
liftSimpleBot (Bot bot) = Bot
  $ \(rid, i) s -> fmap (fmap (fmap ((rid, ) . mkMsg))) $ bot (viewBody i) s

viewBody :: Event -> T.Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: T.Text -> Event
mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg
                                                             TextType
                                                             Nothing
                                                             Nothing

--------------------------------------------------------------------------------
-- Text Bot
--------------------------------------------------------------------------------

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type TextBot m s = Bot m s T.Text [T.Text]

repl :: IO (Server IO [T.Text] T.Text)
repl = do
  -- Read the user's input
  putStr "<<< "
  hFlush stdout
  (T.pack -> input) <- getLine

  return $ Server $ (input,) $ \outputs -> do
    -- Print the bot's responses
    traverse_ (putStrLn . T.unpack . (">>> " <>)) outputs

    -- Do it again
    repl

annihilate :: Monad m => Behavior m i o -> Server m o i -> Fix m
annihilate (Behavior a) (Server (i, b)) = Fix $ do
  BotAction { nextState = a', responses = o } <- a i
  b' <- b o
  return $ annihilate a' b'

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop
