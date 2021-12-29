{-# OPTIONS -fdefer-typed-holes -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
module CofreeBot.Plugins where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Data.Coerce (coerce)
import Data.Foldable (fold, asum, traverse_)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Map.Strict qualified as Map
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import Data.Attoparsec.Text as A
import Data.Char (isAlpha, isDigit)
import Data.Bifunctor (Bifunctor(first))
import Data.Text.Lazy.Builder (flush)
import System.IO (stdout, hFlush)

data BotAction s o = BotAction { responses :: o, nextState :: s }
  deriving (Functor)

instance Bifunctor BotAction
  where
  bimap f g (BotAction a b) = BotAction (g a) (f b)

type Bot m s i o = i -> s -> m (BotAction s o)

type SimpleBot s = Bot IO s T.Text [T.Text]

--TODO: FOR SESSIONS
--data SessionState s = SessionState { sessions :: Map.Map Int s }

--sessionify :: Bot m s -> Bot m (SessionState s)
--sessionify evolve (SessionState states) event = do
--  (k, state, event) <- whichSessionIfAnyDoesEventCorrespondTo states event
--  BotAction {..} <- evolve state event
--  pure $ BotAction { responses, nextState = overwrite k nextState states }
--
--whichSessionIfAnyDoesEventCorrespondTo :: SessionState s -> RoomEvent -> (SessionName, s, RoomEvent)
--whichSessionIfAnyDoesEventCorrespondTo = undefined
   
-------------------------
--- Calculator Bot ---
-------------------------

type VarName = T.Text

data Expr = Var VarName | Val Int | Add Expr Expr | Mult Expr Expr | Neg Expr

data Statement = Let T.Text Expr | Print Expr

type Program = NE.NonEmpty Statement

instance Show Expr where
  showsPrec p = \case
    Var x -> shows $ T.unpack x
    Val n -> shows n
    x `Add` y -> showParen (p >= 6) $ (showsPrec 6 x) . (" + " ++) . (showsPrec 6 y)
    x `Mult` y -> showParen (p >= 7) $ (showsPrec 7 x) . (" * " ++) . (showsPrec 7 y)
    Neg x -> shows $ "- " <> show x

(|*|) :: Applicative f => f a -> f b -> f (a, b)
(|*|) = liftA2 (,)

infixr |*|

pattern (:&) :: a -> b -> (a, b)
pattern a :& b = (a, b)

{-# COMPLETE (:&) #-}

infixr :&

infixOp :: Parser a -> Parser b -> Parser T.Text -> Parser (a, b)
infixOp p1 p2 pop =
  "(" |*| p1 |*| some space |*| pop |*| some space |*| p2 |*| ")" <&>
    \(_oparen :& e1 :& _space :& _plus :& _morespace :& e2 :& _cparen) -> (e1, e2)

parseVarName :: Parser VarName
parseVarName = fmap (\(l, ls) -> l `T.cons` ls) $ letter |*| A.takeWhile (liftA2 (||) isAlpha isDigit)

parseExpr :: Parser Expr
parseExpr = asum
  [ fmap (uncurry Add) $ (parseExpr `infixOp` parseExpr) $ "+"
  , fmap (uncurry Mult) $ (parseExpr `infixOp` parseExpr) $ "*"
  , Neg <$> ("-" *> parseExpr)
  , fmap Val $ decimal
  , fmap Var $ parseVarName
  ]

-- $> import Data.Attoparsec.Text

-- $> parseOnly parseExpr "((11 + x1) + 13)"

parseStmt :: Parser Statement
parseStmt = asum
  [ parseVarName |*| some space |*| ":=" |*| some space |*| parseExpr
    <&> \(var :& _ :& _ :& _ :& expr) -> Let var expr
  , Print <$> parseExpr
  ]

parseProgram :: Parser Program
parseProgram =
  parseStmt |*| (([] <$ endOfInput) <|> endOfLine *> parseStmt `sepBy` endOfLine)
  <&> uncurry (NE.:|)

-- $> parseOnly parseProgram "x := ((11 + 12) + 13)\nx + 1"

data CalcError = LookupError T.Text
  deriving Show

type CalcState = Map.Map T.Text Int

data CalcResp = Log Expr Int

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

arithBot :: Bot IO CalcState Program (Either CalcError [CalcResp])
arithBot program state =
  let f :: Either CalcError (((), [CalcResp]), CalcState) -> BotAction CalcState (Either CalcError [CalcResp])
      f = \case
         Left err -> BotAction (Left err) state
         Right ((_, resp), state') -> BotAction (Right resp) state'
  in fmap f $ interpretProgram' program state

data ParseError = ParseError String

printTxt :: Either CalcError [CalcResp] -> [T.Text]
printTxt = \case
  Left err -> pure $ T.pack $ show err
  Right resps -> resps <&> \(Log expr n) ->
    T.pack $ show expr <> " = " <> show n

parseTxt :: T.Text -> Either ParseError Program
parseTxt = first ParseError . parseOnly parseProgram

arithBot' :: SimpleBot CalcState
arithBot' msg state = case parseTxt msg of
  Left (ParseError err) -> pure $ BotAction ["Failed to parse msg: \"" <> msg <> "\". Error message was: \"" <> T.pack err <> "\"."] state
  Right program -> fmap (fmap printTxt) $ arithBot program state

runSimpleBot :: forall s. SimpleBot s -> s -> IO ()
runSimpleBot bot = go
  where
  go :: s -> IO ()
  go state = do
    putStr "<<< "
    hFlush stdout
    input <- getLine
    BotAction {..} <- bot (T.pack input) state
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState

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


