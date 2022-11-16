{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module CofreeBot.Bot where

import           CofreeBot.Utils
import           CofreeBot.Utils.ListT
import           Control.Applicative
import qualified Control.Arrow                 as Arrow
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Lens                   ( (^.)
                                                , _Just
                                                , ifolded
                                                , view, (&)
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor                 ( Bifunctor(..) )
import           Data.Functor                   ( (<&>) )
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Data.Profunctor
import           Data.Profunctor.Traversing
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.These
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
newtype Bot s i o = Bot { runBot :: s -> i -> (o, s) }
  deriving Functor
  --  (Functor, Applicative, Monad, MonadState s, MonadReader i)
  --via StateT s (ReaderT i Identity)

instance Applicative (Bot s i)
  where
    pure a = Bot $ \s _i -> (a, s)
    liftA2 f (Bot b1) (Bot b2) = Bot $ \s i ->
      let (a, s') = b1 s i
      in first' (f a) $ b2 s' i

instance Monad (Bot s i)
  where
    return = pure
    Bot b1 >>= f = Bot $ \s i ->
      let (a, s') = b1 s i
      in runBot (f a) s' i

-- | The fixed point of a 'Bot'.
-- 
-- Notice that the @s@ parameter has disapeared. This allows us to
-- hide the state threading when interpreting a 'Bot' with some 'Env'.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Behavior i o = Behavior { runBehavior :: i -> (o, (Behavior i o)) }

instance Functor (Behavior i)
  where
  fmap f (Behavior b) = Behavior $ fmap (bimap f (fmap f)) b

instance Profunctor Behavior
  where
  dimap f g (Behavior b) = Behavior $ dimap f (bimap g (dimap f g)) b

instance Choice Behavior
  where
  left' (Behavior b) = Behavior $ either
    (bimap Left left' . b)
    ((, left' (Behavior b)) . Right)

instance Strong Behavior
  where
  first' (Behavior b) = Behavior $ \(a, c) -> bimap (, c) first' (b a)

-- | Generate the fixed point of @Bot m s i o@ by recursively
-- construction an @s -> Behavior m i o@ action and tupling it with
-- the output @o@ from its parent action.
fixBot :: forall s i o . Bot s i o -> s -> Behavior i o
fixBot (Bot b) = go
 where
  go :: s -> Behavior i o
  go s = Behavior $ \i -> second go (b s i)

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
newtype Env s o i = Env { runEnv :: s -> (i, [o] -> s) } -- NOTE: Do we want to receive a @[o]@ here?
  deriving (Functor)

instance Profunctor (Env s)
  where
  dimap f g (Env env) = Env $ fmap (bimap g (lmap (fmap f))) env

-- | The fixed point of an 'Env'. Like in 'Behavior' we have factored
-- out the @s@ parameter to hide the state threading.
--
-- See 'annihilate' for how this interaction occurs in practice.
newtype Server o i = Server { runServer :: (i, [o] -> Server o i) } -- NOTE: Do we want to receive a @[o]@ here?
  deriving (Functor)

instance Profunctor Server
  where
  dimap f g (Server serve) =
    Server $ bimap g (dimap (fmap f) (dimap f g)) serve

-- | Generate the fixed point of @Env m s o i@ by recursively
-- construction an @s -> Server m o i@ action and tupling it with
-- the output @i@ from its parent action.
fixEnv :: forall s o i . Env s o i -> s -> Server o i
fixEnv (Env b) = go
 where
  go :: s -> Server o i
  go s = Server $ fmap (fmap go) $ b s

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Profunctor (Bot s) where
  dimap f g (Bot bot) = do
    Bot $ \s i -> Arrow.first g $ bot s (f i)

instance Strong (Bot s) where
  first' (Bot bot) = Bot $ \s (a, c) -> Arrow.first (, c) $ bot s a

instance Choice (Bot s) where
  left' (Bot bot) = Bot $ \s ->
    either (Arrow.first Left . bot s) (\c -> (Right c, s))

--------------------------------------------------------------------------------
-- Operations
--------------------------------------------------------------------------------

-- | 'Bot' is an invariant functor on @s@ but our types don't quite
-- fit the @Invariant@ typeclass.
invmapBot :: (s -> s') -> (s' -> s) -> Bot s i o -> Bot s' i o
invmapBot f g (Bot b) = Bot $ \s i -> (b (g s) i) & bimap id f

-- | Given the sum of two bots, produce a bot who receives the sum of
-- the inputs to the input bots and produces a wedge product of their
-- outputs.
nudge
  :: Bot s i o \/ Bot s i' o' -> Bot s (i \/ i') (o \*/ o')
nudge = either
  (\(Bot b) -> Bot $ \s -> either
    (fmap (Arrow.first (Just . Left)) (b s)) (const (Nothing, s))
  )
  (\(Bot b) -> Bot $ \s -> either
    (const (Nothing, s)) (fmap (Arrow.first (Just . Right)) $ b s)
  )

-- | Nudge a bot into the left side of a bot with a summed input and
-- wedge product output.
nudgeLeft :: Bot s i o -> Bot s (i \/ i') (o \*/ o')
nudgeLeft = nudge . Left

-- | Nudge a bot into the right side of a bot with a summed input and
-- wedge product output.
nudgeRight :: Bot s i' o' -> Bot s (i \/ i') (o \*/ o')
nudgeRight = nudge . Right

-- | Tuple the states and outputs of two bots who operate on the same
-- input @i@.
infixr /\
(/\) :: Bot s i o -> Bot s' i o' -> Bot (s /\ s') i (o /\ o')
(/\) (Bot b1) (Bot b2) = Bot $ \(s, s') i -> 
  let (nextState , responses ) = b1 s i
      (nextState', responses') = b2 s' i
  in ((nextState, nextState'), (responses, responses'))

-- | Sum the inputs and outputs of two bots who operate on the same
-- state @s@.
--
-- This allows us to combine the behaviors of two bots such that only
-- one or the other bot will be executed depending on the input
-- provided.
infixr \/
(\/) :: Bot s i o -> Bot s i' o' -> Bot s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) =
  Bot $ \s -> either (first Left . b1 s) (first Right . b2 s)

statelessBot :: (i -> o) -> Bot s i o
statelessBot f = Bot $ \s i -> (f i, s)

--------------------------------------------------------------------------------
-- Matrix Bot
--------------------------------------------------------------------------------

type MatrixBot s = Bot s (RoomID, Event) (RoomID, Event)

liftMatrixIO :: (MonadIO m, MonadError MatrixError m) => MatrixIO x -> m x
liftMatrixIO m = liftEither =<< liftIO m

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path = fmap Just (T.readFile path)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

matrix :: ClientSession -> FilePath -> Server (RoomID, Event) [(RoomID, Event)]
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
  go :: FilterID -> Maybe T.Text -> Server (RoomID, Event) [(RoomID, Event)]
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

    pure $ \outputs -> Server $ do
      gen <- newStdGen
      let txnIds = TxnID . T.pack . show <$> randoms @Int gen
      liftIO $ zipWithM_ (uncurry $ sendMessage session) outputs txnIds

      -- Update since file
      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)

      -- Do it again
      runServer $ go filterId (Just newSince)

-- | Map the input and output of a 'MatrixBot' to allow for simple
-- 'T.Text' I/O.
simplifyMatrixBot :: MatrixBot s -> TextBot s
simplifyMatrixBot (Bot bot) = Bot $ \s i ->
  first (viewBody . snd) $ bot s (RoomID mempty, mkMsg i)

liftSimpleBot :: TextBot s -> MatrixBot s
liftSimpleBot (Bot bot) = Bot $ \s (rid, i) ->
  first ((rid,) . mkMsg) $ bot s (viewBody i)

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
type TextBot s = Bot s T.Text T.Text

repl :: Server T.Text T.Text
repl = Server $ do
  -- Read the user's input
  liftIO $ do
    putStr "<<< "
    hFlush stdout
  (T.pack -> input) <- liftIO getLine

  pure $ \outputs -> Server $ do
    forM_ outputs $ \output -> do
      -- Print the bot's responses
      liftIO $ putStrLn $ T.unpack $ ">>> " <> output

    -- Do it again
    runServer repl

-- | Collapse a @Server m o i@ with a @Bahavior m i o@ to create a
-- monadic action @m@.
--annihilate :: Monad m => Server m o i -> Behavior m i o -> Fix m
--annihilate (Server server) b@(Behavior botBehavior) = Fix $ do
--  (i, nextServer) <- server
--  xs              <- fromListT $ botBehavior i
--  let o       = fmap fst $ xs
--      server' = nextServer o
--  pure $ annihilate server' $ case xs of
--    [] -> b
--    _  -> snd $ last xs

loop :: Monad m => Fix m -> m x
loop (Fix x) = x >>= loop
