module CofreeBot.Bot.Behaviors.Calculator
  ( calculatorBot
  , simplifyCalculatorBot
  , printCalcOutput
  ) where

--------------------------------------------------------------------------------

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           CofreeBot.Utils
import           CofreeBot.Utils.ListT          ( toListT )
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor                 ( first )
import           Data.Bitraversable
import           Data.Functor
import           Data.Profunctor
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

type CalculatorOutput = Either CalcError [CalcResp]
type CalculatorBot = Bot IO CalcState Program CalculatorOutput

calculatorBot :: CalculatorBot
calculatorBot = do
  program <- ask
  state (interpretProgram program)

parseErrorBot :: Monad m => Bot m s ParseError T.Text
parseErrorBot = Bot $ \s ParseError {..} ->
  let errorMsg =
        "Failed to parse msg: \""
          <> parseInput
          <> "\". Error message was: \""
          <> parseError
          <> "\"."
  in  pure (errorMsg, s)

simplifyCalculatorBot
  :: forall m s
   . Monad m
  => Bot m s Program (Either CalcError [CalcResp])
  -> Bot m s T.Text T.Text
simplifyCalculatorBot (Bot bot) =
  dimap parseProgram indistinct $ parseErrorBot \/ serializedBot -- rmap (_ . printCalcOutput) bot
 where
  serializedBot :: Bot m s Program T.Text
  serializedBot = Bot $ \s i -> do
    result <- bot s i
    let resps = bitraverse id pure $ first printCalcOutput result
    toListT resps

printCalcOutput :: Either CalcError [CalcResp] -> [T.Text]
printCalcOutput = \case
  Left  err   -> pure $ T.pack $ show err
  Right resps -> resps <&> \case
    Log e n -> T.pack $ show e <> " = " <> show n
