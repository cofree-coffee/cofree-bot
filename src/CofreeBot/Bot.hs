module CofreeBot.Bot where

import CofreeBot.Utils
import Control.Arrow qualified as Arrow
import Control.Category qualified as Cat
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Bifunctor
import Data.Foldable
import Data.Functor ((<&>))
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Text qualified as T
import Network.Matrix.Client (RoomEvent)
import System.IO (stdout, hFlush)

data BotAction s o = BotAction { responses :: o, nextState :: s }
  deriving (Functor)

instance Bifunctor BotAction where
  bimap f g (BotAction a b) = BotAction (g a) (f b)

type KBot = (Type -> Type) -> Type -> Type -> Type -> Type
-- | A 'Bot' maps from some input type 'i' and a state 's' to an
-- output type 'o' and a state 's'
type Bot :: KBot
newtype Bot m s i o = Bot { runBot :: i -> s -> m (BotAction s o) }

invmapBot :: Functor m => (s -> s') -> (s' -> s) -> Bot m s i o -> Bot m s' i o
invmapBot f g (Bot b) = Bot $ \i s -> (b i (g s)) <&> bimap f id

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
  first' (Bot bot) = Bot $ \(a, c) -> fmap (fmap (,c)) . bot a

-- | A 'MatrixBot' maps from 'RoomEvent' to '[RoomEvent]'
type MatrixBot s = Bot IO s RoomEvent [RoomEvent]

-- | A 'SimpleBot' maps from 'Text' to '[Text]'
type SimpleBot s = Bot IO s T.Text [T.Text]

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

nudge :: Applicative m => Bot m s i o \/ Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudge = either
  (\(Bot b) ->
    Bot $ either
      ((fmap . fmap . fmap . fmap) (Just . Left) $ b)
      (const $ \s -> pure $ BotAction Nothing s))
  (\(Bot b) ->
    Bot $ either
      (const $ \s -> pure $ BotAction Nothing s)
      ((fmap . fmap . fmap . fmap) (Just . Right) $ b))

nudgeLeft :: Applicative m => Bot m s i o -> Bot m s (i \/ i') (o \?/ o')
nudgeLeft = nudge . Left

nudgeRight :: Applicative m => Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudgeRight = nudge . Right

(\/) :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ either
  ((fmap . fmap . fmap) Left . b1)
  ((fmap . fmap . fmap) Right . b2)

same :: Either x x -> x
same = either id id

-- | We can use 'dimap' to convert from one 'Bot' type to
-- another. This allows us to construct complex buts from simpler
-- bots. For example we can factor out the particularities of
-- 'Network.Matrix.Client' when constructing a bot.
simpleBotToMatrixBot :: (RoomEvent -> T.Text) -> (T.Text -> RoomEvent) -> SimpleBot s -> MatrixBot s
simpleBotToMatrixBot to from = dimap to (fmap from)

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure $ BotAction (f i) s

runSimpleBot :: forall s. SimpleBot s -> s -> IO ()
runSimpleBot bot = go
  where
  go :: s -> IO ()
  go state = do
    putStr "<<< "
    hFlush stdout
    input <- getLine
    BotAction {..} <- runBot bot (T.pack input) state
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState

fixedPoint :: Bot m s i o -> s -> i -> m o
fixedPoint (Bot bot) = undefined

--TODO: For Mapping Simple Bots to Matrix Bots
-- parseRoomEvent :: RoomEvent -> Either ParseError Program
-- parseRoomEvent roomEvent =
--   let t = roomEvent ^. _reContent . _EventRoomMessage . _RoomMessageText . _mtBody
--   in _ t
-- printResponses :: Either CalcError [CalcResp] -> [Event]
-- printResponses = \case
--   Left err ->
--     let msgTxt = (MessageText (T.pack $ show err) TextType Nothing Nothing)
--         event = EventRoomMessage $ RoomMessageText msgTxt
--     in pure $ event
--   Right resps -> resps <&> \(Log expr n) ->
--     let txt = T.pack $ show expr <> " = " <> show n
--         msgTxt = (MessageText txt TextType Nothing Nothing)
--     in EventRoomMessage $ RoomMessageText msgTxt

--------------------------------------------------------------------------------
-- Session
--------------------------------------------------------------------------------

data SessionState s = SessionState { sessions :: Map.Map Int s }

type Sessionized :: KBot -> KBot
type Sessionized bot m s i o = bot m s (SessionInput i) (Int, o)

data SessionInput i = StartSession | InteractWithSession Int i
data SessionOutput o = SessionOutput Int o

parseSessionInput :: Parser i -> Parser (SessionInput i)
parseSessionInput = undefined

runSession :: (Bot m s i o -> Bot m s T.Text T.Text) -> Sessionized Bot m (SessionState s) i o -> Bot m (SessionState s) T.Text T.Text
runSession f bot = Bot $ \i (SessionState s) -> do
  case parseOnly (parseSessionInput undefined) i of
    Left err -> _
    Right sessionInput -> _

type Serialize m s i o = Bot m s (SessionInput i) (SessionOutput o) -> Sessionized Bot m s (SessionInput T.Text) (SessionOutput T.Text)

-- | Lift a 'Bot' into a 'SessionBot'. 
sessionize :: (Monoid s, Monad m) => Bot m s i o -> Sessionized Bot m (SessionState s) i o
sessionize = _
--sessionize (Bot bot) = Bot $ \(k, i) states -> do
--  let state = findOrCreateSession states k
--  BotAction {..} <- bot i state
--  pure $ BotAction { responses = (k, responses), nextState = SessionState $ Map.insert k nextState (sessions states) }

findOrCreateSession :: Monoid s => SessionState s -> Int -> s
findOrCreateSession states k =
  case findSession states k of
    Just s -> s
    Nothing -> mempty

findSession :: SessionState s -> Int -> Maybe s
findSession (SessionState states) k = Map.lookup k states
