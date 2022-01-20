module CofreeBot.Bot.Behaviors.Calculator
  ( calculatorBot
  , simplifyCalculatorBot
  , printCalcOutput
  ) where

--------------------------------------------------------------------------------

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           CofreeBot.Utils
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Profunctor
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

calculatorBot :: Bot IO CalcState Statement (CalcError \/ CalcResp)
calculatorBot = do
  statement <- ask
  state $ execCalculator statement

parseErrorBot :: Monad m => Bot m s ParseError T.Text
parseErrorBot = pureStatelessBot $ \ParseError {..} ->
  "Failed to parse msg: \""
    <> parseInput
    <> "\". Error message was: \""
    <> parseError
    <> "\"."

simplifyCalculatorBot
  :: Monad m
  => Bot m s Program (Either CalcError CalcResp)
  -> Bot m s T.Text T.Text
simplifyCalculatorBot bot =
  dimap parseProgram indistinct $ parseErrorBot \/ rmap printCalcOutput bot

printCalcOutput :: Either CalcError CalcResp -> T.Text
printCalcOutput = \case
  Left  err       -> T.pack $ show err
  Right Ack       -> "*variable saved*"
  Right (Log e n) -> T.pack $ show e <> " = " <> show n
