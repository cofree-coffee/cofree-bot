{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- | Context Transformations for bots
module CofreeBot.Bot.Context
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
    simplifySessionBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils.ListT (emptyListT)
import Control.Applicative
import Control.Arrow qualified as Arrow
import Data.Attoparsec.Text
import Data.Bifunctor (Bifunctor (first))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Profunctor (second')
import Data.Text qualified as T
import Network.Matrix.Client

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

type UserAware :: KBot -> KBot
type UserAware bot m s i o = bot m s (UserID, i) (UserID, o)

-- | Make a bot "user aware" by tensoring its input and output with a 'UserID'.
--
-- Note: This function exists largely to demonstrate how we can
-- manipulate 'Bot' behavior through tensoring.
mkUserAware :: Functor m => Bot m s i o -> RoomAware Bot m s i o
mkUserAware = second'

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

data Nue = New | Use | End

parseSessionInfo :: Parser i -> Parser (SessionInput i)
parseSessionInfo p = do
  keyword <- New <$ "new" <|> Use <$ "use" <|> End <$ "end"
  case keyword of
    New -> pure StartSession
    Use -> do
      _ <- space
      n <- decimal <* ": "
      i <- p
      -- endOfLine
      pure $ InteractWithSession n i
    End -> do
      _ <- space
      n <- decimal
      pure $ EndSession n

-- | Sessionized bots require a parsable input and printable output.
--
-- Given a printer @o -> T.Text@ and a @Parser i@, convert the
-- sessionized bot into a 'TextBot' which can then be further composed
-- with other bots.
simplifySessionBot ::
  forall m s i o.
  (Show s, Monad m) =>
  (o -> T.Text) ->
  Parser i ->
  Bot m s (SessionInput i) (SessionOutput o) ->
  TextBot m s
simplifySessionBot tshow p (Bot bot) = Bot $ \s i -> do
  case to i of
    Left _ -> emptyListT
    Right si -> fmap (Arrow.first from) $ bot s si
  where
    to :: T.Text -> Either T.Text (SessionInput i)
    to = fmap (first T.pack) $ parseOnly $ parseSessionInfo p

    from :: SessionOutput o -> T.Text
    from = \case
      SessionOutput n o ->
        "Session '" <> T.pack (show n) <> "' Output:\n" <> tshow o
      SessionStarted n -> "Session Started: '" <> T.pack (show n) <> "'."
      SessionEnded n -> "Session Ended: '" <> T.pack (show n) <> "'."
      InvalidSession n -> "Invalid Session: '" <> T.pack (show n) <> "'."
