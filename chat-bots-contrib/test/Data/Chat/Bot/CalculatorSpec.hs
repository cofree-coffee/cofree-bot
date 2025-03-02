{-# LANGUAGE QuasiQuotes #-}

module Data.Chat.Bot.CalculatorSpec where

--------------------------------------------------------------------------------

import Data.Chat.Bot (fixBot)
import Data.Chat.Bot.Calculator
import Data.Chat.Bot.Serialization qualified as S
import Scripts (mkScript)
import Test.Hspec (Spec, describe, it)
import TestServer (conformsToScript)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Calculator Bot" $ do
    let bot = S.applySerializer calculatorBot calculatorSerializer
    it "performs arithmetic" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>(1 + 2)
        <<<1 + 2 = 3
        >>>(2 * 3)
        <<<2 * 3 = 6
        >>>((2 * 3) + 1)
        <<<2 * 3 + 1 = 7
        |]

    it "can store values in state" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>x := (1 + 2)
        <<<*variable saved*
        >>>x
        <<<"x" = 3
        |]
