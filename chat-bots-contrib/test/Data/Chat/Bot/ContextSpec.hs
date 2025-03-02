{-# LANGUAGE QuasiQuotes #-}

module Data.Chat.Bot.ContextSpec where

--------------------------------------------------------------------------------

import Data.Chat.Bot (fixBot)
import Data.Chat.Bot.Calculator
import Data.Chat.Bot.Context (sessionSerializer, sessionize)
import Data.Chat.Bot.Serialization qualified as S
import Scripts (mkScript)
import Test.Hspec (Spec, describe, it)
import TestServer (conformsToScript)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Sessionized Bot" $ do
    let bot = S.applySerializer (sessionize mempty calculatorBot) (sessionSerializer calculatorSerializer)
    it "can instantiate a session" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>new
        <<<Session Started: '0'.
        |]

    it "can delete a session" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>new
        <<<Session Started: '0'.
        >>>end 0
        <<<Session Ended: '0'.
        |]

    it "preserves bot behavior" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>new
        <<<Session Started: '0'.
        >>>use 0: (1 + 2)
        <<<Session '0' Output:
        1 + 2 = 3
        |]

    it "tracks multiple sessions" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>new
        <<<Session Started: '0'.
        >>>new
        <<<Session Started: '1'.
        >>>use 0: x := (1 + 2)
        <<<Session '0' Output:
        *variable saved*
        >>>use 1: x := 42
        <<<Session '1' Output:
        *variable saved*
        >>>use 0: x
        <<<Session '0' Output:
        "x" = 3
        >>>use 1: x
        <<<Session '1' Output:
        "x" = 42
        |]

    it "ignores bodies of text beginning with session keywords" $ do
      fixBot bot mempty
        `conformsToScript` [mkScript|
        >>>new body of text that happens to begin with the word new
        >>>use body of text that begins with the word use
        >>>end body of text that begins with the word end
        |]
