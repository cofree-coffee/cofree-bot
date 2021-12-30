module CofreeBot.Plugins.Calculator where

import CofreeBot.Bot
import CofreeBot.Plugins.Calculator.Language
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State hiding (state)
import Control.Monad.Writer
import Data.Coerce (coerce)
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Profunctor
import Data.Text qualified as T
import System.IO (stdout, hFlush)

-- | Evaluate an expression in our arithmetic language
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

-- | Interpret a language statement into response.
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
  StdOut expr -> do
    val <- eval expr
    tell [Log expr val]
  StdErr err -> tell [LogErr err]

type CalculatorM = WriterT [CalcResp] (StateT CalcState (ExceptT CalcError IO))

-- | Interpret a list of statements 
interpretProgram :: Program -> CalculatorM ()
interpretProgram program = fmap fold $ traverse interpret program

runProgram :: Program -> CalcState -> IO (Either CalcError (((), [CalcResp]), CalcState))
runProgram = coerce interpretProgram

type CalculatorBot = Bot IO CalcState Program (Either CalcError [CalcResp])

calculatorBot :: CalculatorBot
calculatorBot = Bot $ \program state ->
  let f :: Either CalcError (((), [CalcResp]), CalcState) -> BotAction CalcState (Either CalcError [CalcResp])
      f = \case
         Left err -> BotAction (Left err) state
         Right ((_, resp), state') -> BotAction (Right resp) state'
  in fmap f $ runProgram program state

calculatorBotToSimpleBot :: CalculatorBot -> SimpleBot CalcState
calculatorBotToSimpleBot = dimap to from
  where
    to :: T.Text -> Program
    to msg = 
      case parseTxt msg of
        Left (ParseError err) -> pure $ StdErr $ "Failed to parse msg: \"" <> msg <> "\". Error message was: \"" <> T.pack err <> "\"."
        Right program -> program

    from :: Either CalcError [CalcResp] -> [T.Text]
    from = printTxt

simpleCalculatorBot :: SimpleBot CalcState
simpleCalculatorBot = Bot $ \msg state ->
  case parseTxt msg of
    Left (ParseError err) -> pure $ BotAction ["Failed to parse msg: \"" <> msg <> "\". Error message was: \"" <> T.pack err <> "\"."] state
    Right program -> fmap (fmap printTxt) $ runBot calculatorBot program state

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
