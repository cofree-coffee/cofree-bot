module CofreeBot.Bot where

import           CofreeBot.Utils
import qualified Control.Arrow                 as Arrow
import qualified Control.Category              as Cat
import           Control.Lens            hiding ( from
                                                , to
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable
import           Data.IORef
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Data.Profunctor
import qualified Data.Text                     as T
import           Network.Matrix.Client
import           Network.Matrix.Client.Lens
import           System.Directory               ( createDirectoryIfMissing )
import           System.IO
import           System.Random

--------------------------------------------------------------------------------
-- Kinds
--------------------------------------------------------------------------------

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data BotAction s o = BotAction
  { responses :: o
  , nextState :: s
  }
  deriving Functor

-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot
newtype Bot m s i o = Bot { runBot :: i -> s -> m (BotAction s o) }

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
  arr f = rmap f (Cat.id)
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
invmapBot f g (Bot b) = Bot $ \i s -> (b i (g s)) <&> bimap f id

nudge
  :: Applicative m
  => Bot m s i o \/ Bot m s i' o'
  -> Bot m s (i \/ i') (o \?/ o')
nudge = either
  (\(Bot b) -> Bot $ either ((fmap . fmap . fmap . fmap) (Just . Left) $ b)
                            (const $ \s -> pure $ BotAction Nothing s)
  )
  (\(Bot b) -> Bot $ either (const $ \s -> pure $ BotAction Nothing s)
                            ((fmap . fmap . fmap . fmap) (Just . Right) $ b)
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
  Bot $ \i s -> maybe (pure (BotAction mempty s)) (flip bot s) $ f i

--------------------------------------------------------------------------------
-- Matrix Bot
--------------------------------------------------------------------------------

type MatrixBot m s = Bot m s (RoomID, RoomEvent) [MatrixAction]

data MatrixMessage = MatrixMessage { mmRid :: RoomID, mmEvent :: Event }
data MatrixReply = MatrixReply { mrRid :: RoomID, mrOriginal :: RoomEvent, mrMessage :: MessageText }
data MatrixAction = SendMessage MatrixMessage | SendReply MatrixReply

runMatrixAction :: ClientSession -> TxnID -> MatrixAction -> MatrixIO EventID
runMatrixAction session txnId = \case
  SendMessage (MatrixMessage {..}) -> sendMessage session mmRid mmEvent txnId
  SendReply (MatrixReply {..}) -> let
    event = mkReply mrRid mrOriginal mrMessage
    in sendMessage session mrRid event txnId

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

          invites :: [T.Text]
          invites = fmap fst $ Map.toList $ syncResult ^. _srRooms . _Just . _srrInvite . ifolded

          roomEvents :: Map.Map T.Text [RoomEvent]
          roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

          events :: [(RoomID, RoomEvent)]
          events = Map.foldMapWithKey
            (\rid es -> fmap ((RoomID rid, ) . id) es)
            roomEvents

      liftIO $ print syncResult
      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)
      liftIO $ acceptInvites invites
      traverse_ (go ref) events
 where
  acceptInvites :: [T.Text] -> IO ()
  acceptInvites invites = traverse_ (joinRoom session) invites
  
  go :: MonadIO m => IORef s -> (RoomID, RoomEvent) -> m ()
  go ref input = do
    gen            <- newStdGen
    state          <- liftIO $ readIORef ref
    BotAction {..} <- liftIO $ runBot bot input state
    liftIO $ writeIORef ref nextState
    let txnIds = (TxnID . T.pack . show <$> randoms @Int gen)
    liftIO $ sequence_ $ zipWith (runMatrixAction session) txnIds responses 

simplifyMatrixBot :: Monad m => MatrixBot m s -> TextBot m s
simplifyMatrixBot (Bot bot) = Bot $ \i s -> do
  BotAction {..} <- bot (RoomID mempty, mkRoomEvent i) s
  pure $ BotAction (fmap (viewBody . mkEvent) responses) s
  where
    mkRoomEvent :: T.Text -> RoomEvent
    mkRoomEvent msg =
      RoomEvent (EventRoomMessage $ mkMsg msg) mempty (EventID mempty) (Author mempty)

    mkEvent :: MatrixAction -> Event
    mkEvent = \case
      SendMessage MatrixMessage{..} -> mmEvent
      SendReply MatrixReply{..} -> EventRoomReply (EventID mempty) (RoomMessageText mrMessage)


liftSimpleBot :: Functor m => TextBot m s -> MatrixBot m s
liftSimpleBot (Bot bot) = Bot
  $ \(rid, i) s -> fmap (fmap  (fmap (SendMessage . mkMsg rid))) $ bot (viewBody i) s
 where
  viewBody :: RoomEvent -> T.Text
  viewBody = (view (_reContent . _EventRoomMessage . _RoomMessageText . _mtBody))

  mkMsg :: RoomID -> T.Text -> MatrixMessage
  mkMsg rid' msg =
    MatrixMessage rid' $ EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing

viewBody :: Event -> T.Text
viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

mkMsg :: T.Text -> RoomMessage
mkMsg msg = RoomMessageText $ MessageText msg TextType Nothing Nothing

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
  go state = do
    putStr "<<< "
    hFlush stdout
    input          <- getLine
    BotAction {..} <- runBot bot (T.pack input) state
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState
