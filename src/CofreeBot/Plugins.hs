module CofreeBot.Plugins where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Network.Matrix.Client
import Network.Matrix.Client.Lens

instance Bifunctor PluginAction where

data PluginAction s o = PluginAction { responses :: o, nextState :: s }
  deriving (Functor)

type Plugin m s i o = i -> s -> m (PluginAction s o)

--TODO: FOR SESSIONS
--data SessionState s = SessionState { sessions :: Map.Map Int s }

--sessionify :: Plugin m s -> Plugin m (SessionState s)
--sessionify evolve (SessionState states) event = do
--  (k, state, event) <- whichSessionIfAnyDoesEventCorrespondTo states event
--  PluginAction {..} <- evolve state event
--  pure $ PluginAction { responses, nextState = overwrite k nextState states }
--
--whichSessionIfAnyDoesEventCorrespondTo :: SessionState s -> RoomEvent -> (SessionName, s, RoomEvent)
--whichSessionIfAnyDoesEventCorrespondTo = undefined
   
-------------------------
--- Calculator Plugin ---
-------------------------

data Expr = Var T.Text | Val Int | Add Expr Expr | Mult Expr Expr | Neg Expr

instance Show Expr where
  showsPrec p = \case
    Var x -> shows $ T.unpack x
    Val n -> shows n
    x `Add` y -> showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
    x `Mult` y -> showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
    Neg x -> shows $ "- " <> show x

data Statement = Let T.Text Expr | Print Expr
type Program = NE.NonEmpty Statement

data CalcError = LookupError T.Text
  deriving Show

eval ::
  ( MonadError CalcError m
  , MonadState CalcState m
  )
  => Expr -> m Int
eval = \case
  Var bndr -> do
    state <- get
    maybe (throwError $ LookupError bndr) pure $ Map.lookup bndr state  
  Val i -> pure i
  Add x y -> liftA2 (+) (eval x) (eval y)
  Mult x y -> liftA2 (*) (eval x) (eval y)
  Neg x -> fmap negate $ eval x
  
interpret ::
  ( MonadError CalcError m
  , MonadState CalcState m
  , MonadWriter [CalcResp] m
  )
  => Statement -> m ()
interpret = \case
  Let bndr expr -> do
    val <- eval expr
    modify (Map.insert bndr val)
  Print expr -> do
    val <- eval expr
    tell $ [Log expr val]

type AppM = WriterT [CalcResp] (StateT CalcState (ExceptT CalcError IO))

interpretProgram :: Program -> AppM ()
interpretProgram program = fmap fold $ traverse interpret program

interpretProgram' :: Program -> CalcState -> IO (Either CalcError (((), [CalcResp]), CalcState))
interpretProgram' = coerce interpretProgram
    
type CalcState = Map.Map T.Text Int

data CalcResp = Log Expr Int

arithBot :: Plugin IO CalcState Program (Either CalcError [CalcResp])
arithBot program state = 
  let f :: Either CalcError (((), [CalcResp]), CalcState) -> PluginAction CalcState (Either CalcError [CalcResp])
      f = \case
         Left err -> PluginAction (Left err) state
         Right ((_, resp), state') -> PluginAction (Right resp) state'
  in fmap f $ interpretProgram' program state

type MatrixPlugin s = Plugin IO s T.Text [T.Text]

arithBot' :: MatrixPlugin CalcState
arithBot' msg state = case parseTxt msg of
  Left err -> pure $ PluginAction ["Failed to parse msg: " <> msg] state
  Right program -> fmap (fmap printTxt) $ arithBot program state

printTxt :: Either CalcError [CalcResp] -> [T.Text]
printTxt = \case
  Left err -> pure $ T.pack $ show err
  Right resps -> resps <&> \(Log expr n) ->
    T.pack $ show expr <> " = " <> show n

parseTxt :: T.Text -> Either ParseError Program 
parseTxt t = _

printResponses :: Either CalcError [CalcResp] -> [Event]
printResponses = \case
  Left err ->
    let msgTxt = (MessageText (T.pack $ show err) TextType Nothing Nothing)
        event = EventRoomMessage $ RoomMessageText msgTxt
    in pure $ event
  Right resps -> resps <&> \(Log expr n) ->
    let txt = T.pack $ show expr <> " = " <> show n
        msgTxt = (MessageText txt TextType Nothing Nothing)
    in EventRoomMessage $ RoomMessageText msgTxt

data ParseError = ParseError

parseRoomEvent :: RoomEvent -> Either ParseError Program
parseRoomEvent roomEvent =
  let t = roomEvent ^. _reContent . _EventRoomMessage . _RoomMessageText . _mtBody
  in undefined t

