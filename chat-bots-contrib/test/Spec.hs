{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

--------------------------------------------------------------------------------

import Data.Chat.Bot (Behavior, Bot (..), fixBot)
import Data.Chat.Bot.Calculator
import Data.Chat.Bot.Context (sessionSerializer, sessionize)
import Data.Chat.Bot.Hello
import Data.Chat.Bot.Serialization qualified as S
import Data.Text (Text, pack)
import Scripts (Script, mkScript)
import Test.Hspec (Spec, describe, hspec, it, shouldNotBe)
import TestServer (Completion (..), conformsToScript, conformsToScript')

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  scriptedTestsSpec
  helloBotSpec
  calculatorBotSpec
  sessionizedBotSpec

scriptedTestsSpec :: Spec
scriptedTestsSpec = describe "Scripted tests" $ do
  let myBehavior :: forall m. (Monad m) => Behavior m Text Text
      myBehavior = flip fixBot True $ Bot $ \s _ -> return (pack $ show s, not s)

  it "can deal with bots that respond correctly" $ do
    myBehavior
      `conformsToScript` [mkScript|
      >>>hello
      <<<True
      >>>whatever
      <<<False
      |]

  it "can deal with bots that respond incorrectly" $ do
    let script :: Script
        script =
          [mkScript|
                 >>>hello
                 <<<True
                 >>>whatever
                 <<<True
                 |]
    result <- myBehavior `conformsToScript'` script
    result `shouldNotBe` Passed

helloBotSpec :: Spec
helloBotSpec =
  describe "Hello Bot" $ do
    let bot = S.applySerializer helloBot helloBotSerializer
    it "responds to precisely its trigger phrase" $ do
      fixBot bot ()
        `conformsToScript` [mkScript|
        >>>cofree-bot
        <<<Are you talking to me, punk?
        |]

    it "responds to its trigger phrase embedded in a sentence" $ do
      fixBot bot ()
        `conformsToScript` [mkScript|
        >>>hows it going cofree-bot
        <<<Are you talking to me, punk?
        |]

calculatorBotSpec :: Spec
calculatorBotSpec =
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

sessionizedBotSpec :: Spec
sessionizedBotSpec =
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
