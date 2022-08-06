module CofreeBot.Bot.Behaviors.Calculator where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           CofreeBot.Utils
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Functor
import           Data.Profunctor
import qualified Data.Text                     as T

type CalculatorOutput = Either CalcError [CalcResp]
type CalculatorBot = Bot IO CalcState Program CalculatorOutput

calculatorBot :: CalculatorBot
calculatorBot = do
  program <- ask
  state (interpretProgram program)

parseErrorBot :: Applicative m => Bot m s ParseError [T.Text]
parseErrorBot = pureStatelessBot $ \ParseError {..} ->
  pure
    $  "Failed to parse msg: \""
    <> parseInput
    <> "\". Error message was: \""
    <> parseError
    <> "\"."

simplifyCalculatorBot
  :: Applicative m
  => Bot m s Program (Either CalcError [CalcResp])
  -> Bot m s T.Text [T.Text]
simplifyCalculatorBot bot =
  dimap parseProgram indistinct $ parseErrorBot \/ rmap printCalcOutput bot

printCalcOutput :: Either CalcError [CalcResp] -> [T.Text]
printCalcOutput = \case
  Left  err   -> pure $ T.pack $ show err
  Right resps -> resps <&> \case
    Log e n -> T.pack $ show e <> " = " <> show n
