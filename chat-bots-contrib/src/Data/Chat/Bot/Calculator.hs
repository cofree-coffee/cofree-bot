module Data.Chat.Bot.Calculator
  ( -- * Bot
    calculatorBot,
    calculatorBot',
    printer,

    -- * Serializer
    calculatorSerializer,

    -- * Language
    module Language,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Reader
import Control.Monad.State
import Data.Attoparsec.Text (parseOnly)
import Data.Chat.Bot
import Data.Chat.Bot.Calculator.Language as Language
import Data.Chat.Bot.Serialization (TextSerializer)
import Data.Chat.Bot.Serialization qualified as S
import Data.Chat.Utils
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

calculatorBot :: Bot IO CalcState Statement (CalcError \/ CalcResp)
calculatorBot = ask >>= state . execCalculator

calculatorBot' :: Bot IO CalcState Text Text
calculatorBot' = S.applySerializer calculatorBot calculatorSerializer

--------------------------------------------------------------------------------

calculatorSerializer :: TextSerializer (CalcError \/ CalcResp) Statement
calculatorSerializer = S.Serializer {parser, printer}

parser :: Text -> Maybe Statement
parser = either (const Nothing) Just . parseOnly statementP

printer :: Either CalcError CalcResp -> Text
printer = \case
  Left err -> Text.pack $ show err
  Right Ack -> "*variable saved*"
  Right (Log e n) -> Text.pack $ show e <> " = " <> show n
