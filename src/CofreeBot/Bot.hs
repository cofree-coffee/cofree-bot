{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module CofreeBot.Bot
  ( -- * Bot
    KBot
  , Bot(..)
  , invmapBot
  , mapMaybeBot
  , nudge
  , nudgeLeft
  , nudgeRight
  , (/\)
  , (\/)
  , pureStatelessBot
  , emptyBot
  , hoistBot
  , liftEffect
  , -- * Behavior
    Behavior(..)
  , fixBot
  , -- * Env
    Env(..)
  , fixEnv
  , -- * Server
    Server(..)
  , annihilate
  , loop
  , -- * Matrix Bot
    MatrixBot
  , matrix
  , simplifyMatrixBot
  , liftSimpleBot
  , -- * Text Bot
    TextBot
  , repl
  ) where

--------------------------------------------------------------------------------

import           CofreeBot.Utils
import qualified Control.Arrow                 as Arrow
import qualified Control.Category              as Cat
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Lens                   ( (^.)
                                                , _Just
                                                , ifolded
                                                , view
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Fix                       ( Fix(..) )
import           Data.Foldable
import           Data.Functor                   ( (<&>) )
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

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot
newtype Bot m s i o = Bot { runBot :: s -> i -> m (o, s) }
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i, MonadIO)
  via StateT s (ReaderT i m)

instance Functor f => Profunctor (Bot f s) where
  dimap f g (Bot bot) = do
    Bot $ \s i -> fmap (Arrow.first g) $ bot s (f i)

instance Functor f => Strong (Bot f s) where
  first' (Bot bot) = Bot $ \s (a, c) -> fmap (Arrow.first (, c)) $ bot s a

instance Applicative f => Choice (Bot f s) where
  left' (Bot bot) = Bot $ \s ->
    either (fmap (Arrow.first Left) . bot s) (\c -> pure $ (,) (Right c) s)

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

-- | 'Bot' is an invariant functor on @s@ but our types don't quite
-- fit the @Invariant@ typeclass.
invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \s i -> (b (g s) i) <&> bimap id f

mapMaybeBot
  :: (Applicative m, Monoid o) => (i -> Maybe i') -> Bot m s i' o -> Bot m s i o
mapMaybeBot f (Bot bot) =
  Bot $ \s i -> maybe (pure ((,) mempty s)) (bot s) $ f i

-- | Given the sum of two bots, produce a bot who receives the sum of
-- the inputs to the input bots and produces a wedge product of their
-- outputs.
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

-- | Nudge a bot into the left side of a bot with a summed input and
-- wedge product output.
nudgeLeft :: Applicative m => Bot m s i o -> Bot m s (i \/ i') (o \?/ o')
nudgeLeft = nudge . Left

-- | Nudge a bot into the right side of a bot with a summed input and
-- wedge product output.
nudgeRight :: Applicative m => Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudgeRight = nudge . Right

-- | Tuple the states and outputs of two bots who operate on the same
-- input @i@.
infixr /\
(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \(s, s') i -> do
  (nextState , responses ) <- b1 s i
  (nextState', responses') <- b2 s' i
  pure $ (,) (nextState, nextState') (responses, responses')

-- | Sum the inputs and outputs of two bots who operate on the same
-- state @s@.
--
-- This allows us to combine the behaviors of two bots such that only
-- one or the other bot will be executed depending on the input
-- provided.
infixr \/
(\/)
  :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ \s ->
  either (fmap (Arrow.first Left) . b1 s) (fmap (Arrow.first Right) . b2 s)

-- | Construct a 'Bot' which maps from @i@ to @o@ without using its
-- state @s@ or monadic action @m@.
pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \s i -> pure $ (,) (f i) s

-- | Lift the @Monoid o@ unit value into @Bot m s i o@.
emptyBot :: (Monoid o, Applicative m) => Bot m s i o
emptyBot = pureStatelessBot $ const mempty

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Bot m s i o@ to @Bot n s i o@
hoistBot :: (forall x . m x -> n x) -> Bot m s i o -> Bot n s i o
hoistBot f (Bot b) = Bot $ fmap (fmap f) b

-- | Lift a monadic effect @m o@ into a @Bot m s i o@.
liftEffect :: Monad m => m o -> Bot m s i o
liftEffect m = Bot $ \s _ -> do
  o <- m
  pure (o, s)

--------------------------------------------------------------------------------

-- | The fixed point of a 'Bot'.
-- 
-- Notice that the @s@ parameter has disapeared. This allows us to
-- hide the state threading when interpreting a 'Bot' with some 'Env'.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Behavior m i o = Behavior { runBehavior :: i -> m (o, (Behavior m i o)) }

instance Functor m => Functor (Behavior m i)
  where
  fmap f (Behavior b) = Behavior $ fmap (fmap (bimap f (fmap f))) b

instance Functor m => Profunctor (Behavior m)
  where
  dimap f g (Behavior b) = Behavior $ dimap f (fmap (bimap g (dimap f g))) b

instance Applicative m => Choice (Behavior m)
  where
  left' (Behavior b) = Behavior $ either
    (fmap (bimap Left left') . b)
    (pure . (, left' (Behavior b)) . Right)

instance Functor m => Strong (Behavior m)
  where
  first' (Behavior b) = Behavior $ \(a, c) -> fmap (bimap (, c) first') (b a)

instance Monad m => Traversing (Behavior m)
  where
  -- TODO: write wander instead for efficiency
  traverse' b = Behavior $ \is ->
    fmap (uncurry (,) . fmap traverse') $ flip runStateT b $ traverse
      (\i -> StateT $ \(Behavior b') ->
        fmap (\(responses, nextState) -> (responses, nextState)) $ b' i
      )
      is

-- | Generate the fixed point of @Bot m s i o@ by recursively
-- construction an @s -> Behavior m i o@ action and tupling it with
-- the output @o@ from its parent action.
fixBot :: forall m s i o . Functor m => Bot m s i o -> s -> Behavior m i o
fixBot (Bot b) = go
 where
  go :: s -> Behavior m i o
  go s = Behavior $ \i -> second go <$> b s i

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
type Env :: KBot
newtype Env m s o i = Env { runEnv :: s -> m (i, o -> s) }
  deriving (Functor)

instance Functor m => Profunctor (Env m s)
  where
  dimap f g (Env env) = Env $ fmap (fmap (bimap g (lmap f))) env

-- | Generate the fixed point of @Env m s o i@ by recursively
-- construction an @s -> Server m o i@ action and tupling it with
-- the output @i@ from its parent action.
fixEnv :: forall m s o i . Functor m => Env m s o i -> s -> Server m o i
fixEnv (Env b) = go
 where
  go :: s -> Server m o i
  go s = Server $ fmap (fmap (fmap go)) $ b s

--------------------------------------------------------------------------------

-- | The fixed point of an 'Env'. Like in 'Behavior' we have factored
-- out the @s@ parameter to hide the state threading.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Server m o i = Server { runServer :: m (i, o -> Server m o i) }
  deriving (Functor)

instance Functor m => Profunctor (Server m)
  where
  dimap f g (Server serve) =
    Server $ fmap (bimap g (dimap f (dimap f g))) serve

-- | Collapse a @Server m o i@ with a @Bahavior m i o@ to create a
-- monadic action @m@.
annihilate :: Monad m => Server m o i -> Behavior m i o -> Fix m
annihilate (Server server) (Behavior botBehaviorbot) = Fix $ do
  (i, nextServer  ) <- server
  (o, botBehavior') <- botBehaviorbot i
  let server' = nextServer o
  pure $ annihilate server' botBehavior'

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop

--------------------------------------------------------------------------------

type MatrixBot m s = Bot m s (RoomID, Event) [(RoomID, Event)]

liftMatrixIO :: (MonadIO m, MonadError MatrixError m) => MatrixIO x -> m x
liftMatrixIO m = liftEither =<< liftIO m

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path = fmap Just (T.readFile path)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

-- | A Matrix 'Server' for connecting a 'Bot' to the Matrix protocol.
matrix
  :: forall m
   . (MonadError MatrixError m, MonadIO m)
  => ClientSession
  -> FilePath
  -> Server m [(RoomID, Event)] [(RoomID, Event)]
matrix session cache = Server $ do
  -- Setup cache
  liftIO $ createDirectoryIfMissing True cache
  since    <- liftIO $ readFileMaybe $ cache <> "/since_file"

  -- Log in
  userId   <- liftMatrixIO $ getTokenOwner session
  filterId <- liftMatrixIO $ createFilter session userId messageFilter

  -- Start looping
  runServer $ go filterId since

 where
  go
    :: FilterID -> Maybe T.Text -> Server m [(RoomID, Event)] [(RoomID, Event)]
  go filterId since = Server $ do
    -- Get conversation events
    syncResult <- liftMatrixIO
      $ sync session (Just filterId) since (Just Online) (Just 1000)

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

    pure $ (events, ) $ \outputs -> Server $ do
      -- Send the bot's responses
      gen <- newStdGen
      let txnIds = TxnID . T.pack . show <$> randoms @Int gen
      liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

      -- Update since file
      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)

      -- Do it again
      runServer $ go filterId (Just newSince)

-- | Map the input and output of a 'MatrixBot' to allow for simple
-- 'T.Text' I/O.
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

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type TextBot m s = Bot m s T.Text [T.Text]

-- | A repl-style 'Server' for interacting with a 'Bot'.
repl :: Server IO [T.Text] T.Text
repl = Server $ do
  -- Read the user's input
  putStr "<<< "
  hFlush stdout
  (T.pack -> input) <- getLine

  pure $ (input, ) $ \outputs -> Server $ do
    -- Print the bot's responses
    traverse_ (putStrLn . T.unpack . (">>> " <>)) outputs

    -- Do it again
    runServer repl
