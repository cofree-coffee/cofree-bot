{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- | Context Transformations for bots
module Data.Chat.Bot.Context
  ( -- * Room Awareness
    RoomAware,
    mkRoomAware,

    -- * User Awareness
    UserAware,
    mkUserAware,

    -- * Session
    SessionState (..),
    SessionInput (..),
    SessionOutput (..),
    sessionize,
    sessionSerializer,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative
import Data.Attoparsec.Text
import Data.Chat.Bot
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Profunctor (second')
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Matrix.Client (RoomID, UserID)

--------------------------------------------------------------------------------

type RoomAware :: KBot -> KBot
type RoomAware bot m s i o = bot m s (RoomID, i) (RoomID, o)

-- | Make a bot "room aware" by tensoring its input and output with a 'RoomID'.
--
-- Note: This function exists largely to demonstrate how we can
-- manipulate 'Bot' behavior through tensoring.
mkRoomAware :: Functor m => Bot m s i o -> RoomAware Bot m s i o
mkRoomAware = second'

--------------------------------------------------------------------------------

type UserAware bot m s i o = bot m s (UserID, i) (UserID, o)

-- | Make a bot "user aware" by tensoring its input and output with a 'UserID'.
--
-- Note: This function exists largely to demonstrate how we can
-- manipulate 'Bot' behavior through tensoring.
mkUserAware :: Functor m => Bot m s i o -> RoomAware Bot m s i o
mkUserAware = second'

--------------------------------------------------------------------------------

-- | Enable sessions for a 'Bot'.
--
-- A sessionized 'Bot' can be interacted with using the commands @new@, @use@, and @end@:
--
-- @new@ - Instantiate a new session. The bot will return a
-- @SessionStarted Int@ response with a session id for session
-- interaction.
--
-- @use n: i@ - Interact with a bot session, where @n@ is a session id
-- and @i@ is an ordinary input for the non-sessionized 'Bot'.
--
-- @end n@ - Terminate session @n@.
sessionize ::
  Monad m =>
  s ->
  Bot m s i o ->
  Bot m (SessionState s) (SessionInput i) (SessionOutput o)
sessionize defaultState (Bot bot) = Bot $ \(SessionState s) si -> case si of
  StartSession -> do
    let k = freshSessionKey s
    pure $
      (,) (SessionStarted k) (SessionState $ IntMap.insert k defaultState s)
  EndSession k -> do
    pure $ (,) (SessionEnded k) (SessionState $ IntMap.delete k s)
  InteractWithSession k i -> case IntMap.lookup k s of
    Nothing -> pure $ (,) (InvalidSession k) (SessionState s)
    Just s' -> do
      (responses, nextState) <- bot s' i
      pure $
        (,)
          (SessionOutput k responses)
          (SessionState $ IntMap.insert k nextState s)

--------------------------------------------------------------------------------

-- | A map of states @s@ used to track sessions in a "sessionized" bot.
newtype SessionState s = SessionState {sessions :: IntMap s}
  deriving newtype (Show, Read, Semigroup, Monoid)

freshSessionKey :: IntMap a -> Int
freshSessionKey state = case IntMap.lookupMax state of
  Nothing -> 0
  Just (k, _) -> k + 1

-- | Expand the input type @i@ to include session interaction meta commands.
data SessionInput i = InteractWithSession Int i | StartSession | EndSession Int

-- | Expand the output type @o@ to include session interaction meta commands.
data SessionOutput o = SessionOutput Int o | SessionStarted Int | SessionEnded Int | InvalidSession Int

--------------------------------------------------------------------------------

sessionSerializer :: TextSerializer o i -> TextSerializer (SessionOutput o) (SessionInput i)
sessionSerializer S.Serializer {parser = parser', printer = printer'} =
  S.Serializer {parser = parser parser', printer = printer printer'}

printer :: (o -> Text) -> SessionOutput o -> Text
printer p = \case
  SessionOutput n o ->
    "Session '" <> Text.pack (show n) <> "' Output:\n" <> p o
  SessionStarted n -> "Session Started: '" <> Text.pack (show n) <> "'."
  SessionEnded n -> "Session Ended: '" <> Text.pack (show n) <> "'."
  InvalidSession n -> "Invalid Session: '" <> Text.pack (show n) <> "'."

parser :: (Text -> Maybe i) -> Text -> Maybe (SessionInput i)
parser p = either (const Nothing) Just . parseOnly (sessionizedParser p)

data Nue = New | Use | End

sessionizedParser :: (Text -> Maybe i) -> Parser (SessionInput i)
sessionizedParser p = do
  keyword <- New <$ "new" <|> Use <$ "use" <|> End <$ "end"
  case keyword of
    New -> pure StartSession
    Use -> do
      _ <- space
      n <- decimal <* ": "
      mi <- fmap p takeText
      -- endOfLine
      case mi of
        Just i -> pure $ InteractWithSession n i
        Nothing -> fail "bad parse"
    End -> do
      _ <- space
      n <- decimal
      pure $ EndSession n
