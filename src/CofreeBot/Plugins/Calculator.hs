{-# LANGUAGE TypeOperators #-}
module CofreeBot.Plugins.Calculator where

import CofreeBot.Bot
import CofreeBot.Plugins.Calculator.Language
import Data.Profunctor
import Data.Text qualified as T
import Data.Functor

type CalculatorBot = Bot IO CalcState Program (Either CalcError [CalcResp])

calculatorBot :: CalculatorBot
calculatorBot = Bot $ \program state ->
  fmap (uncurry BotAction) $ interpretProgram program state

parseErrorBot :: Applicative m => Bot m s ParseError T.Text
parseErrorBot = pureStatelessBot $ \ParseError {..} ->
  "Failed to parse msg: \"" <> parseInput <> "\". Error message was: \"" <> parseError <> "\"."

simpleCalculatorBot :: SimpleBot CalcState
simpleCalculatorBot
  = dimap parseProgram same
  $ rmap (:[]) parseErrorBot \/ rmap printTxt calculatorBot
  where
  printTxt :: Either CalcError [CalcResp] -> [T.Text]
  printTxt = \case
    Left err -> pure $ T.pack $ show err
    Right resps -> resps <&> \case
      Log e n -> T.pack $ show e <> " = " <> show n
