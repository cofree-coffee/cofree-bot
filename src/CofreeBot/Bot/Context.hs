{-# LANGUAGE MultiParamTypeClasses #-}
-- | Context Transformations for bots
module CofreeBot.Bot.Context where

import CofreeBot.Bot
import CofreeBot.Bot.Simple
import Network.Matrix.Client
import Data.Map.Strict qualified as Map
import Data.Attoparsec.Text
import Data.Text qualified as T
import Data.Bifunctor (bimap)
import Control.Applicative
import Data.Profunctor (second')

--------------------------------------------------------------------------------
-- Room Awareness
--------------------------------------------------------------------------------

type RoomAware :: KBot -> KBot
type RoomAware bot m s i o = bot m s (RoomID, i) (RoomID, o)

-- | 'mkRoomAware' makes a bot "room aware"
mkRoomAware :: Functor m => Bot m s i o -> RoomAware Bot m s i o
mkRoomAware = second'

--------------------------------------------------------------------------------
-- User Awareness
--------------------------------------------------------------------------------

type UserAware :: KBot -> KBot
type UserAware bot m s i o = bot m s (UserID, i) (UserID, o)

-- | 'mkUserAware' makes a bot "room aware"
mkUserAware :: Functor m => Bot m s i o -> RoomAware Bot m s i o
mkUserAware = second'

--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------

newtype SessionState s = SessionState { sessions :: Map.Map Int s }
  deriving (Show, Semigroup, Monoid)

freshSessionKey :: Map.Map Int a -> Int
freshSessionKey state =
  case Map.lookupMax state of
    Nothing -> 0
    Just (k, _) -> k + 1

data SessionInput i = InteractWithSession Int i | StartSession | EndSession Int
data SessionOutput o = SessionOutput Int o | SessionStarted Int | SessionEnded Int | InvalidSession Int

-- | Transform a 'Bot' into a 'Sessionized' 'Bot'. 
sessionize :: Monad m => s -> Bot m s i o -> Bot m (SessionState s) (SessionInput i) (SessionOutput o)
sessionize defaultState (Bot bot) = Bot $ \si (SessionState s) ->
  case si of
    StartSession -> do
      let k = freshSessionKey s 
      pure $ BotAction (SessionStarted k) (SessionState $ Map.insert k defaultState s)
    EndSession k -> do
      pure $ BotAction (SessionEnded k) (SessionState $ Map.delete k s)
    InteractWithSession k i ->
      case Map.lookup k s of
        Nothing -> pure $ BotAction (InvalidSession k) (SessionState s)
        Just s' -> do
          BotAction{..} <- bot i s'
          pure $ BotAction (SessionOutput k responses) (SessionState $ Map.insert k nextState s) 

data Nue = New | Use | End

parseSessionInfo :: Parser i -> Parser (SessionInput i)
parseSessionInfo p = do
  keyword <- New <$ "new" <|> Use <$ "use" <|> End <$ "end"
  case keyword of
    New -> pure $ StartSession
    Use -> do
      _ <- space
      n <- decimal <* ": "
      i <- p
      --endOfLine
      pure $ InteractWithSession n i
    End -> do
      _ <- space
      n <- decimal
      pure $ EndSession n
    
simplifySessionBot ::
  forall m s i o. (Show s, Applicative m) =>
  (o -> T.Text) ->
  Parser i ->
  Bot m s (SessionInput i) (SessionOutput o) ->
  TextBot m s
simplifySessionBot tshow p (Bot bot) = Bot $ \i s -> do
    case to i of
      Left _ -> pure $ BotAction [] s
      Right si -> fmap (fmap from) $ bot si s
  where
    to :: T.Text -> Either T.Text (SessionInput i)
    to = fmap (bimap T.pack id) $ parseOnly $ (parseSessionInfo p)

    from :: SessionOutput o -> [T.Text]
    from = \case
      SessionOutput n o -> pure $ "Session '" <> T.pack (show n) <> "' Output:\n" <> tshow o
      SessionStarted n -> pure $ "Session Started: '" <> T.pack (show n) <> "'."
      SessionEnded n -> pure $ "Session Ended: '" <> T.pack (show n) <> "'."
      InvalidSession n -> pure $ "Invalid Session: '" <> T.pack (show n) <> "'."
