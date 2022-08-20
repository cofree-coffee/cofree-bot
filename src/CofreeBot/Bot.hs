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
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor (Bifunctor(..))
import           Data.Foldable
import           Data.Functor ((<&>))
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Data.Profunctor
import           Data.Profunctor.Traversing
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Network.Matrix.Client
import           Network.Matrix.Client.Lens
import           System.Directory               ( createDirectoryIfMissing )
import           System.IO
import           System.IO.Error                ( isDoesNotExistError )
import           System.Random


--------------------------------------------------------------------------------
-- Kinds
--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Fix f = Fix { runFix :: f (Fix f) }

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot

newtype Bot m s i o = Bot { runBot :: s -> i -> m (o, s) }
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i, MonadIO)
  via StateT s (ReaderT i m)

newtype Behavior m i o = Behavior { runBehavior :: i -> m (o, (Behavior m i o)) }

instance Functor m => Functor (Behavior m i)
  where
  fmap f (Behavior b) = Behavior $ fmap (fmap (bimap f (fmap f))) b

instance Functor m => Profunctor (Behavior m)
  where
  dimap f g (Behavior b) = Behavior $ dimap f (fmap (bimap g (dimap f g))) b

instance Applicative m => Choice (Behavior m)
  where
  left' (Behavior b) = Behavior $ either (fmap (bimap Left left') . b) (pure . (, left' (Behavior b)) . Right)

instance Functor m => Strong (Behavior m)
  where
  first' (Behavior b) = Behavior $ \(a, c) -> fmap (bimap (, c) first') (b a)

instance Monad m => Traversing (Behavior m)
  where
  -- TODO: write wander instead for efficiency

  traverse' b = Behavior $ \is ->
    fmap (uncurry (,) . fmap traverse')
    $ flip runStateT b
    $ traverse (\i -> StateT $ \(Behavior b') -> fmap (\(responses, nextState) -> (responses, nextState)) $ b' i) is

fixBot :: Functor m => Bot m s i o -> s -> Behavior m i o
fixBot (Bot b) = go
  where
  go s = Behavior $ \i -> second go <$> b s i

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

instance Monad m => Cat.Category (Bot m s) where
  id = ask

  Bot f . Bot g = do
    i        <- ask
    s        <- get
    (b, s' ) <- liftEffect $ g s i
    (c, s'') <- liftEffect $ f s' b
    put s''
    pure c

instance Monad m => Arrow.Arrow (Bot m s) where
  arr f = rmap f Cat.id
  first = first'

instance Functor f => Profunctor (Bot f s) where
  dimap f g (Bot bot) = do
    Bot $ \s i -> fmap (Arrow.first g) $ bot s (f i)

instance Functor f => Strong (Bot f s) where
  first' (Bot bot) = Bot $ \s (a, c) -> fmap (Arrow.first (, c)) $ bot s a

instance Applicative f => Choice (Bot f s) where
  left' (Bot bot) = Bot $ \s ->
    either (fmap (Arrow.first Left) . bot s) (\c -> pure $ (,) (Right c) s)

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | 'Bot' is an invariant functor on 's' but we cannot write an instance in Haskell.
invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \s i -> (b (g s) i) <&> bimap id f

liftEffect :: Monad m => m o -> Bot m s i o
liftEffect m = Bot $ \s _ -> do
  o <- m
  pure (o, s)

nudge
  :: Applicative m
  => Bot m s i o \/ Bot m s i' o'
  -> Bot m s (i \/ i') (o \?/ o')
nudge = either
  (\(Bot b) -> Bot $ \s -> either
    (fmap (fmap (Arrow.first (Just . Left))) $ b s)
    (const $ pure $ (,) Nothing s)
  )
  (\(Bot b) -> Bot $ \s -> either
    (const $ pure $ (,) Nothing s)
    (fmap (fmap (Arrow.first (Just . Right))) $ b s)
  )

nudgeLeft :: Applicative m => Bot m s i o -> Bot m s (i \/ i') (o \?/ o')
nudgeLeft = nudge . Left

nudgeRight :: Applicative m => Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudgeRight = nudge . Right

infixr /\
(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \(s, s') i -> do
  (nextState , responses ) <- b1 s i
  (nextState', responses') <- b2 s' i
  pure $ (,) (nextState, nextState') (responses, responses')

infixr \/
(\/)
  :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ \s ->
  either (fmap (Arrow.first Left) . b1 s) (fmap (Arrow.first Right) . b2 s)

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \s i -> pure $ (,) (f i) s

mapMaybeBot
  :: (Applicative m, Monoid o) => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
mapMaybeBot f (Bot bot) =
  Bot $ \s i -> maybe (pure ((,) mempty s)) (bot s) $ f i

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
simplifyMatrixBot (Bot bot) = Bot $ \s i -> do
  (responses, nextState) <- bot s (RoomID mempty, mkMsg i)
  pure $ (,) (fmap (viewBody . snd) $ responses) nextState

liftSimpleBot :: Functor m => TextBot m s -> MatrixBot m s
liftSimpleBot (Bot bot) = Bot $ \s (rid, i) ->
  fmap (Arrow.first (fmap (\t -> (rid, mkMsg t)))) $ bot s (viewBody i)

viewBody :: Event -> T.Text
viewBody = view (_EventRoomMessage . _RoomMessageText . _mtBody)

mkMsg :: T.Text -> Event
mkMsg msg =
  EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing

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
  (o, a') <- a i
  b' <- b o
  return $ annihilate a' b'

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop
