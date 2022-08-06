module CofreeBot.Bot where

import           CofreeBot.Utils
import qualified Control.Arrow                 as Arrow
import qualified Control.Category              as Cat
import           Control.Exception              ( catch
                                                , throwIO
                                                )
import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable
import           Data.IORef
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


--------------------------------------------------------------------------------
-- Kinds
--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot

newtype Bot m s i o = Bot { runBot :: s -> i -> m (o, s) }
  deriving
    (Functor, Applicative, Monad, MonadState s, MonadReader i, MonadIO)
  via StateT s (ReaderT i m)

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

--------------------------------------------------------------------------------
-- Matrix Bot
--------------------------------------------------------------------------------

type MatrixBot m s = Bot m s (RoomID, Event) [(RoomID, Event)]

readFileMaybe :: String -> IO (Maybe T.Text)
readFileMaybe path = fmap Just (T.readFile path)
  `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e

runMatrixBot
  :: forall s . ClientSession -> String -> MatrixBot IO s -> s -> IO ()
runMatrixBot session cache bot s = do
  ref <- newIORef s
  createDirectoryIfMissing True cache
  since <- readFileMaybe $ cache <> "/since_file"
  void $ runExceptT $ do
    userId   <- ExceptT $ getTokenOwner session
    filterId <- ExceptT $ createFilter session userId messageFilter
    syncPoll session (Just filterId) since (Just Online) $ \syncResult -> do
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

      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)
      liftIO $ print roomEvents
      traverse_ (go ref) events
 where
  go :: MonadIO m => IORef s -> (RoomID, Event) -> m ()
  go ref input = do
    st                     <- liftIO $ readIORef ref
    (responses, nextState) <- liftIO $ runBot bot st input
    liftIO $ writeIORef ref nextState
    gen <- newStdGen
    let txnIds = (TxnID . T.pack . show <$> randoms @Int gen)
    liftIO $ zipWithM_ (uncurry $ sendMessage session)
                                 responses
                                 txnIds

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

-- | An evaluator for running 'TextBots' in 'IO'
runTextBot :: forall s . TextBot IO s -> s -> IO ()
runTextBot bot = go
 where
  go :: s -> IO ()
  go st = do
    putStr "<<< "
    hFlush stdout
    input                  <- getLine
    (responses, nextState) <- runBot bot st (T.pack input)
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState
