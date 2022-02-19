-- | Matrix Protocol Harness
module CofreeBot.Bot.Harness.Matrix where

import           CofreeBot.Bot
import           CofreeBot.MessagingAPI
import           CofreeBot.Utils                ( readFileMaybe )
import           Control.Applicative
import           Control.Lens                   ( (^.)
                                                , _Just
                                                , ifolded
                                                , view
                                                )
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Attoparsec.Text          as A
import           Data.Bifunctor                (first)
import           Data.Coerce
import           Data.Either                   (partitionEithers)
import           Data.Functor
import           Data.Foldable                 (fold, traverse_)
import           Data.List                     (intersperse)
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import           GHC.IORef
import qualified Network.Matrix.Client         as NMC
import           Network.Matrix.Client.Lens
import           System.Directory
import           System.Random
import Control.Lens.Fold (preview)

data Matrix

type MatrixBot m s = Bot m s (MatrixChannel, NMC.RoomEvent) [APIAction Matrix]

data MatrixMessage = MatrixMessage
  { mmRoomAlias :: NMC.RoomAlias
  , mmMessage   :: NMC.MessageText
  }
data MatrixReply = MatrixReply
  { mrRoomAlias :: NMC.RoomAlias
  , mrOriginal  :: NMC.RoomEvent
  , mrMessage   :: NMC.MessageText
  }
data MatrixRoomAction
  = RAJoinRoom NMC.RoomAlias
  -- ^ RoomID or Alias
  | RALeaveRoom NMC.RoomAlias
  -- ^ RoomID
  | RAListRooms NMC.RoomAlias
  -- ^ The RoomID from which the request came

data MatrixAction
  = MASendMessage MatrixMessage
  | MASendReply MatrixReply
  | MARoomAction MatrixRoomAction

data MatrixChannel = RoomID NMC.RoomID | RoomAlias NMC.RoomAlias
  deriving Show

instance MessagingAPI Matrix where
  type Context Matrix = ReaderT NMC.ClientSession IO
  type UserReference Matrix = NMC.UserID
  type UserMetadata Matrix = NMC.Username
  type Channel Matrix = MatrixChannel
  type MessageReference Matrix = NMC.RoomEvent
  -- ^ NOTE: For Matrix, we must use the full RoomEvent as the Identifier
  type MessageContent Matrix = NMC.MessageText

  listMembers
    :: MatrixChannel
    -> ReaderT NMC.ClientSession IO (Map.Map NMC.UserID NMC.Username)
  listMembers _alias = do
    _session <- ask
    -- TODO(SOLOMON):
    pure $ undefined

  parseChannel :: A.Parser MatrixChannel
  parseChannel = do
    chanTy <- fmap Left "!" <|> fmap Right "#"
    iden <- fmap T.pack $ A.many1 (A.letter <|> A.digit)
    void ":"
    host <- fmap (T.pack . fold . intersperse ".") $(A.many1 (A.letter <|> A.digit)) `A.sepBy1` "."
    case chanTy of
     Left _ -> pure $ RoomID $ NMC.RoomID $ "!" <> iden <> ":" <> host
     Right _ -> pure $ RoomAlias $ NMC.RoomAlias $ "#" <> iden <> ":" <> host

  runMessageParser :: A.Parser a -> MessageReference Matrix -> Maybe a
  runMessageParser p roomEvent =
    let msg = viewBody $ NMC.reContent roomEvent
      in either (const Nothing) Just $ A.parseOnly p msg

  messageMentionsBot :: NMC.RoomEvent -> Bool
  messageMentionsBot re = 
    let tag = "<a href=\"https://matrix.to/#/@cofree-bot:cofree.coffee\">cofree-bot</a>"
    in case preview (_reContent . roomMessageOfEvent . _RoomMessageText . _mtFormattedBody . _Just) re of
      Just msg ->
        if tag `T.isInfixOf` msg
          then True
          else False
      Nothing -> False

getRoomNameFromRoomID
  :: NMC.ClientSession
  -> NMC.RoomID
  -> IO (Either NMC.MatrixError (NMC.StateContent 'NMC.RoomName))
getRoomNameFromRoomID session rid =
  NMC.getRoomStateContent session rid NMC.RoomNameType (NMC.StateKey "")

runMatrixAction
  :: NMC.ClientSession
  -> NMC.TxnID
  -> MatrixAction
  -> ExceptT NMC.MatrixError IO ()
runMatrixAction session txnId = \case
  MASendMessage (MatrixMessage {..}) -> do
    rid <- ExceptT $ fmap (fmap NMC.roomID) $ NMC.resolveRoomAlias session
                                                                   mmRoomAlias
    void $ ExceptT $ NMC.sendMessage
      session
      rid
      (NMC.EventRoomMessage $ NMC.RoomMessageText mmMessage)
      txnId
  MASendReply (MatrixReply {..}) -> do
    roomId <- ExceptT $ fmap (fmap NMC.roomID) $ NMC.resolveRoomAlias
      session
      mrRoomAlias
    let event = NMC.mkReply roomId mrOriginal mrMessage
    void $ liftIO $ NMC.sendMessage session roomId event txnId
  MARoomAction (RAJoinRoom (NMC.RoomAlias room)) ->
    void $ ExceptT $ NMC.joinRoom session room
  MARoomAction (RALeaveRoom room) -> do
    roomId <- ExceptT $ fmap (fmap NMC.roomID) $ NMC.resolveRoomAlias session
                                                                      room
    void $ ExceptT $ NMC.leaveRoomById session roomId
  MARoomAction (RAListRooms alias) -> do
    rid <- ExceptT $ fmap (fmap NMC.roomID) $ NMC.resolveRoomAlias session alias
    rooms <- ExceptT $ NMC.getJoinedRooms session
    (errs, names) <- liftIO $ fmap partitionEithers $ traverse
      (getRoomNameFromRoomID session)
      rooms
    liftIO $ print errs
    void $ liftIO $ NMC.sendMessage
      session
      rid
      (printRoomList $ fmap (view _StateContentMRName) names)
      txnId

runMatrix
  :: forall s . NMC.ClientSession -> String -> MatrixBot IO s -> s -> IO ()
runMatrix session cache bot s = do
  ref <- newIORef s
  createDirectoryIfMissing True cache
  since <- readFileMaybe $ cache <> "/since_file"
  void $ runExceptT $ do
    userId   <- ExceptT $ NMC.getTokenOwner session
    filterId <- ExceptT $ NMC.createFilter session userId NMC.messageFilter
    NMC.syncPoll session (Just filterId) since (Just NMC.Online)
      $ \syncResult -> do
          let
            newSince :: T.Text
            newSince = syncResult ^. _srNextBatch

            roomsMap :: Map.Map T.Text NMC.JoinedRoomSync
            roomsMap = syncResult ^. _srRooms . _Just . _srrJoin . ifolded

            invites :: [T.Text]
            invites =
              fmap fst
                $  Map.toList
                $  syncResult
                ^. _srRooms
                .  _Just
                .  _srrInvite
                .  ifolded

            roomEvents :: Map.Map T.Text [NMC.RoomEvent]
            roomEvents = roomsMap <&> view (_jrsTimeline . _tsEvents . _Just)

            --unknownEvents :: Map.Map T.Text [J.Object]
            --unknownEvents = roomEvents <&> fmap (view (_reContent . _EventUnknown))

            events :: [(NMC.RoomID, NMC.RoomEvent)]
            events =
              filter
                  ( (/= "@cofree-bot:cofree.coffee")
                  . NMC.unAuthor
                  . NMC.reSender
                  . snd
                  )
                $ Map.foldMapWithKey
                    (\rid es -> fmap ((NMC.RoomID rid, ) . id) es)
                    roomEvents

          --events' <- traverse (bitraverse (getRoomNameFromRoomID session) pure) events
          liftIO $ print syncResult
          liftIO $ writeFile (cache <> "/since_file") (T.unpack newSince)
          liftIO $ acceptInvites invites
          traverse_ (go ref) (fmap (first RoomID) events)
 where
  acceptInvites :: [T.Text] -> IO ()
  acceptInvites invites = traverse_ (NMC.joinRoom session) invites

  processMatrixActions
    :: [NMC.TxnID] -> [APIAction Matrix] -> IO [NMC.MatrixError]
  processMatrixActions _txnIds _responses = error "TODO"
  --processMatrixActions txnIds responses =
  --  let actions = zipWith (runMatrixAction session) txnIds responses
  --  in fmap (fst . partitionEithers) $ traverse runExceptT actions


  go :: MonadIO m => IORef s -> (MatrixChannel, NMC.RoomEvent) -> m ()
  go ref input = do
    gen            <- newStdGen
    state          <- liftIO $ readIORef ref
    BotAction {..} <- liftIO $ runBot bot input state
    liftIO $ writeIORef ref nextState
    let txnIds = (NMC.TxnID . T.pack . show <$> randoms @Int gen)
    errors <- liftIO $ processMatrixActions txnIds responses
    liftIO $ print errors

viewBody :: NMC.Event -> T.Text
viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

mkMessageText :: T.Text -> NMC.MessageText
mkMessageText msg = NMC.MessageText msg NMC.TextType Nothing Nothing

mkRoomMessage :: T.Text -> NMC.RoomMessage
mkRoomMessage = NMC.RoomMessageText . mkMessageText

mkEvent :: APIAction Matrix -> NMC.Event
mkEvent = \case
  APIAction (MkMessage _chan mc) -> NMC.EventRoomMessage $ NMC.RoomMessageText mc
  APIAction (MkReply _chan mr mc) -> NMC.EventRoomReply (NMC.reEventId mr) $ NMC.RoomMessageText mc
  APIAction (JoinRoom _chan) -> NMC.EventRoomMessage $ NMC.RoomMessageText $ mkMessageText $ "Bot joined" <> T.pack (show _chan)
  APIAction (LeaveRoom _chan) -> NMC.EventRoomMessage $ NMC.RoomMessageText $ mkMessageText $ "Bot left" <> T.pack (show _chan)

printRoomList :: [NMC.MRName] -> NMC.Event
printRoomList rids =
  let msg =
        "Cofree-Bot is available in:\n"
          <> foldMap ((\rid -> "- " <> rid <> "\n") . coerce) rids
  in  NMC.EventRoomMessage $ mkRoomMessage msg
