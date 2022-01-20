module CofreeBot.Bot.Behaviors.Calculator where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           CofreeBot.Utils
import           CofreeBot.Utils.ListT
import           CofreeBot.Utils.ListT          ( toListT )
import           Data.Bifunctor                 ( first )
import           Data.Bitraversable
import           Data.Bitraversable
import           Data.Function                  ( (&) )
import           Data.Functor                   ( )
import           Data.Profunctor
import qualified Data.Text                     as T
import           Data.Tuple                     ( swap )

type CalculatorOutput = Either CalcError CalcResp
type CalculatorBot = Bot IO CalcState Program CalculatorOutput

calculatorBot :: CalculatorBot
calculatorBot = Bot $ \state program -> ListT $ do
  let o = interpretProgram program state
  runListT $ fmap (uncurry (,)) $ bitraverse (traverse toListT) pure o

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
  Left  err   -> T.pack $ show err
  Right resps -> resps & \case
    Log e n -> T.pack $ show e <> " = " <> show n
