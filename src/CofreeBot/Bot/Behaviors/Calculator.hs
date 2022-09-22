module CofreeBot.Bot.Behaviors.Calculator where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           CofreeBot.Utils
import           Data.Function                  ( (&) )
import           Data.Functor                   ( )
import           Data.Profunctor
import qualified Data.Text                     as T
import CofreeBot.Utils.ListT (emptyListT)
import Control.Monad.IO.Class

type CalculatorOutput = Either CalcError CalcResp

calculatorBot :: Bot IO CalcState Statement CalculatorOutput
calculatorBot = Bot $ \state program -> do
  let (o, s) = interpretProgram program state
  liftIO $ print s
  case sequence o of
    Nothing -> emptyListT
    Just resp -> pure (resp, s)

parseErrorBot :: Monad m => Bot m s ParseError T.Text
parseErrorBot = pureStatelessBot $ \ParseError {..} ->
  "Failed to parse msg: \""
    <> parseInput
    <> "\". Error message was: \""
    <> parseError
    <> "\"."

simplifyCalculatorBot
  :: Monad m
  => Bot m s Statement (Either CalcError CalcResp)
  -> Bot m s T.Text T.Text
simplifyCalculatorBot bot =
  dimap parseProgram indistinct $ emptyBot \/ rmap printCalcOutput bot

printCalcOutput :: Either CalcError CalcResp -> T.Text
printCalcOutput = \case
  Left  err   -> T.pack $ show err
  Right resps -> resps & \case
    Log e n -> T.pack $ show e <> " = " <> show n
