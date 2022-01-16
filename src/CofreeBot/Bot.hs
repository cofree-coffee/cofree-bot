{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module CofreeBot.Bot where

import           CofreeBot.Utils
import qualified Control.Arrow                 as Arrow
import qualified Control.Category              as Cat
import           Control.Lens            hiding ( from
                                                , to
                                                , re
                                                )
import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable
import           Data.IORef
import           Data.Kind
import qualified Data.Map.Strict               as Map
import           Data.Profunctor
import qualified Data.Text                     as T
import qualified Network.Matrix.Client         as NMC
import           Network.Matrix.Client.Lens
import           System.Directory               ( createDirectoryIfMissing )
import           System.IO
import           System.Random
import Data.Void
import Control.Lens.Unsound

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

roomMessageOfEvent :: Traversal' NMC.Event NMC.RoomMessage
roomMessageOfEvent = _EventRoomMessage `adjoin` (_EventRoomReply . _2) `adjoin` (_EventRoomEdit . _2)
    
--------------------------------------------------------------------------------
-- Bot Messaging API
--------------------------------------------------------------------------------

class MessagingAPI api where
  type Channel api = (r :: Type) | r -> api
  -- ^ The destination channel for them message. Eg., RoomID on Matrix.
  type MessageReference api = (r :: Type) | r -> api
  -- ^ The identifier for the incoming message.
  type MessageContent api :: Type
  -- ^ The message content to be sent out.
  type Action api = (r :: Type) | r -> api
  -- ^ The type of actions available on the api.

  messageIsMention :: MessageReference api -> Bool
  sendMessage :: Channel api -> MessageContent api -> Action api
  reply :: Channel api -> MessageReference api -> MessageContent api -> Action api

--------------------------------------------------------------------------------
-- Matrix Bot
--------------------------------------------------------------------------------

data Matrix

type MatrixBot m s = Bot m s (NMC.RoomID, NMC.RoomEvent) [MatrixAction]

data MatrixMessage = MatrixMessage { mmRid :: NMC.RoomID, mmMessage :: NMC.MessageText }
data MatrixReply = MatrixReply { mrRid :: NMC.RoomID, mrOriginal :: NMC.RoomEvent, mrMessage :: NMC.MessageText }
data MatrixAction = SendMessage MatrixMessage | SendReply MatrixReply

instance MessagingAPI Matrix where
  type Channel Matrix = NMC.RoomID
  type MessageReference Matrix = NMC.RoomEvent 
  -- ^ NOTE: For Matrix, we must use the full RoomEvent as the Identifier
  type MessageContent Matrix = NMC.MessageText 
  type Action Matrix = MatrixAction

  messageIsMention re = 
    let tag = "<a href=\"https://matrix.to/#/@cofree-bot:cofree.coffee\">cofree-bot</a>"
    in case preview (_reContent . roomMessageOfEvent . _RoomMessageText . _mtFormattedBody . _Just) re of
      Just msg ->
        if tag `T.isInfixOf` msg
          then True
          else False
      Nothing -> False

  sendMessage :: NMC.RoomID -> NMC.MessageText -> MatrixAction
  sendMessage rid = SendMessage . MatrixMessage rid

  reply :: NMC.RoomID -> NMC.RoomEvent -> NMC.MessageText -> MatrixAction
  reply rid roomEvent = SendReply . MatrixReply rid roomEvent

runMatrixAction :: NMC.ClientSession -> NMC.TxnID -> MatrixAction -> NMC.MatrixIO NMC.EventID
runMatrixAction session txnId = \case
  SendMessage (MatrixMessage {..}) -> NMC.sendMessage session mmRid (NMC.EventRoomMessage $ NMC.RoomMessageText mmMessage) txnId
  SendReply (MatrixReply {..}) -> let
    event = NMC.mkReply mrRid mrOriginal mrMessage
    in NMC.sendMessage session mrRid event txnId

runMatrixBot
  :: forall s . NMC.ClientSession -> String -> MatrixBot IO s -> s -> IO ()
runMatrixBot session cache bot s = do
  ref <- newIORef s
  createDirectoryIfMissing True cache
  since <- readFileMaybe $ cache <> "/since_file"
  void $ runExceptT $ do
    userId   <- ExceptT $ NMC.getTokenOwner session
    filterId <- ExceptT $ NMC.createFilter session userId NMC.messageFilter
    NMC.syncPoll session (Just filterId) since (Just NMC.Online) $ \syncResult -> do
      let newSince :: T.Text
          newSince = syncResult ^. _srNextBatch

          roomsMap :: Map.Map T.Text NMC.JoinedRoomSync
          roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded

          invites :: [T.Text]
          invites = fmap fst $ Map.toList $ syncResult ^. _srRooms . _Just . _srrInvite . ifolded

          roomEvents :: Map.Map T.Text [NMC.RoomEvent]
          roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

          events :: [(NMC.RoomID, NMC.RoomEvent)]
          events = filter ((/= "@cofree-bot:cofree.coffee") . NMC.unAuthor . NMC.reSender . snd) $ Map.foldMapWithKey
            (\rid es -> fmap ((NMC.RoomID rid, ) . id) es)
            roomEvents

      liftIO $ print syncResult
      liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)
      liftIO $ acceptInvites invites
      traverse_ (go ref) events
 where
  acceptInvites :: [T.Text] -> IO ()
  acceptInvites invites = traverse_ (NMC.joinRoom session) invites
  
  go :: MonadIO m => IORef s -> (NMC.RoomID, NMC.RoomEvent) -> m ()
  go ref input = do
    gen            <- newStdGen
    state          <- liftIO $ readIORef ref
    BotAction {..} <- liftIO $ runBot bot input state
    liftIO $ writeIORef ref nextState
    let txnIds = (NMC.TxnID . T.pack . show <$> randoms @Int gen)
    liftIO $ sequence_ $ zipWith (runMatrixAction session) txnIds responses 

-- | This function throws away all awareness of rooms.
simplifyMatrixBot :: Monad m => MatrixBot m s -> TextBot m s
simplifyMatrixBot (Bot bot) = Bot $ \i s -> do
  BotAction {..} <- bot (NMC.RoomID mempty, mkRoomEvent i) s
  pure $ BotAction (fmap (viewBody . mkEvent) responses) s
  where
    mkRoomEvent :: T.Text -> NMC.RoomEvent
    mkRoomEvent msg =
      NMC.RoomEvent (NMC.EventRoomMessage $ mkRoomMessage msg) mempty (NMC.EventID mempty) (NMC.Author mempty)

liftSimpleBot :: Functor m => TextBot m s -> MatrixBot m s
liftSimpleBot (Bot bot) = Bot $ \(rid, i) s ->
  fmap (fmap (fmap (sendMessage rid . mkMessageText))) $ bot (viewBody $ NMC.reContent i) s

viewBody :: NMC.Event -> T.Text
viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

mkMessageText :: T.Text -> NMC.MessageText
mkMessageText msg = NMC.MessageText msg NMC.TextType Nothing Nothing

mkRoomMessage :: T.Text -> NMC.RoomMessage
mkRoomMessage = NMC.RoomMessageText . mkMessageText

mkEvent :: MatrixAction -> NMC.Event
mkEvent = \case
  SendMessage MatrixMessage{..} -> NMC.EventRoomMessage $ NMC.RoomMessageText mmMessage
  SendReply MatrixReply{..} -> NMC.EventRoomReply (NMC.EventID mempty) (NMC.RoomMessageText mrMessage)

--------------------------------------------------------------------------------
-- Text Bot
--------------------------------------------------------------------------------

data Repl

data TextAction
  = TASendMessage T.Text
  | TAReply T.Text T.Text

instance MessagingAPI Repl where
  type Channel Repl = ()
  type MessageReference Repl = Void 
  -- ^ The Repl protocol does not support replies.
  type MessageContent Repl = T.Text
  type Action Repl = TextAction

  messageIsMention = const False
  sendMessage _ = TASendMessage
  reply _ = absurd

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
