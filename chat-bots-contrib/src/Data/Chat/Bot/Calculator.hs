module Data.Chat.Bot.Calculator
  ( calculatorBot,
    simplifyCalculatorBot,
    printCalcOutput,
    module Language,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Reader
import Control.Monad.State
import Data.Chat.Bot
import Data.Chat.Bot.Calculator.Language as Language
import Data.Chat.Bot.Monoidal
import Data.Chat.Utils
import Data.Profunctor
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

calculatorBot :: Bot IO CalcState Statement (CalcError \/ CalcResp)
calculatorBot = do
  statement <- ask
  state $ execCalculator statement

parseErrorBot :: Monad m => Bot m s ParseError Text
parseErrorBot = pureStatelessBot $ \ParseError {..} ->
  "Failed to parse msg: \""
    <> parseInput
    <> "\". Error message was: \""
    <> parseError
    <> "\"."

simplifyCalculatorBot ::
  Monad m =>
  Bot m s Statement (CalcError \/ CalcResp) ->
  Bot m s Text Text
simplifyCalculatorBot bot =
  dimap parseStatement indistinct $ parseErrorBot \/ rmap printCalcOutput bot

printCalcOutput :: Either CalcError CalcResp -> Text
printCalcOutput = \case
  Left err -> Text.pack $ show err
  Right Ack -> "*variable saved*"
  Right (Log e n) -> Text.pack $ show e <> " = " <> show n
