module CofreeBot.Bot.Behaviors.Calculator (calculatorBot) where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Calculator.Language
import           Data.Functor
import Data.String
import CofreeBot.MessagingAPI
import qualified Data.Attoparsec.Text as A

type CalculatorOutput = Either CalcError [CalcResp]

interpreterBot :: Bot IO CalcState Program CalculatorOutput
interpreterBot = Bot $ \program state ->
  fmap (uncurry BotAction) $ interpretProgram program state

calculatorBot :: forall api. (MessagingAPI api, IsString (MessageContent api)) => Bot IO CalcState (Channel api, MessageReference api) [APIAction api]
calculatorBot = Bot $ \(chan, msg) s -> do
  case runMessageParser parse msg of
    Nothing -> pure $ BotAction [] s
    Just program -> fmap (fmap (prettyPrint chan)) $ runBot interpreterBot program s

parse :: A.Parser Program
parse = "arith:" *> A.many' A.space *> programP

prettyPrint :: IsString (MessageContent api) => Channel api -> Either CalcError [CalcResp] -> [APIAction api]
prettyPrint chan = \case
  Left err -> pure $ APIAction $ MkMessage chan $ fromString $ show err
  Right resp -> resp <&> (\(Log e n) -> APIAction $ MkMessage chan $ fromString $ show e <> " = " <> show n)
