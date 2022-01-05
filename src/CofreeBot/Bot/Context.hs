{-# LANGUAGE MultiParamTypeClasses #-}
-- | Context Transformations for bots
module CofreeBot.Bot.Context where

import Control.Monad (void)
import CofreeBot.Bot
import CofreeBot.Utils
import CofreeBot.Bot.Simple
import Network.Matrix.Client
import Data.Map.Strict qualified as Map
import Data.Attoparsec.Text as A
import Data.Text qualified as T
import Data.Bifunctor (bimap)
import Control.Applicative
import Data.Profunctor (second')
import Data.Char

--------------------------------------------------------------------------------
-- Room Awareness
--------------------------------------------------------------------------------

type RoomAware :: KBot -> KBot
type RoomAware bot m s i o = bot m s (RoomID, i) (RoomID, o)

-- | 'mkRoomAware' makes a bot "room aware"
mkRoomAware :: Functor m => Bot m s i o -> RoomAware Bot m s i o
mkRoomAware = second'

--------------------------------------------------------------------------------
-- Debug
--------------------------------------------------------------------------------

newtype DebugId = DebugId T.Text
  deriving Eq

data DebugInput i = DebugInput { diDebugId :: DebugId, diInput :: i}

debuggize :: (Monoid o, Monad m) => DebugId -> Bot m s i o -> Bot m s (DebugInput i) o
debuggize did (Bot bot) = Bot $ \(DebugInput did' i) s ->
  if did == did'
    then bot i s
    else pure $ BotAction mempty s

simplifyDebugBot ::
  forall m s i o. (Show s, Applicative m) =>
  Printer o ->
  Parser i ->
  Bot m s (DebugInput i) [o] ->
  TextBot m s
simplifyDebugBot printer parser (Bot bot) =
  Bot $ \i s ->
    case to i of
      Left _ -> pure $ BotAction [] s
      Right i' -> fmap (fmap from) $ bot i' s
  where
   to :: T.Text -> Either T.Text (DebugInput i)
   to = fmap (bimap T.pack id) $ parseOnly $ (parseDebugInfo parser)

   from :: [o] -> [T.Text]
   from = fmap printer

parseDebugInfo :: Parser i -> Parser (DebugInput i)
parseDebugInfo p = do
  void "debug"
  void space
  n <- fmap (uncurry T.cons) $ letter |*| A.takeWhile (liftA2 (||) isAlpha isDigit)
  void ":"
  void space
  i <- p
  pure $ DebugInput (DebugId n) i

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
