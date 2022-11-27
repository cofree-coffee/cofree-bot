{-# LANGUAGE QuasiQuotes #-}

module Main where

--------------------------------------------------------------------------------

import CofreeBot.Bot (fixBot)
import CofreeBot.Bot.Behaviors
  ( calculatorBot,
    helloSimpleBot,
    printCalcOutput,
    simplifyCalculatorBot,
  )
import CofreeBot.Bot.Behaviors.Calculator.Language (statementP)
import CofreeBot.Bot.Context (sessionize, simplifySessionBot)
import Scripts (mkScript)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import TestServer (runTestScript)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  helloBotSpec
  calculatorBotSpec
  sessionizedBotSpec

helloBotSpec :: Spec
helloBotSpec =
  describe "Hello Bot" $ do
    let bot = helloSimpleBot
    it "responds to precisely its trigger phrase" $ do
      let scenario =
            [mkScript|
            >>>cofree-bot
            <<<Are you talking to me, punk?
            |]
      result <- runTestScript scenario $ fixBot bot ()
      result `shouldBe` scenario

    it "responds to its trigger phrase embedded in a sentence" $ do
      let scenario =
            [mkScript|
            >>>hows it going cofree-bot
            <<<Are you talking to me, punk?
            |]
      result <- runTestScript scenario $ fixBot bot ()
      result `shouldBe` scenario

calculatorBotSpec :: Spec
calculatorBotSpec =
  describe "Calculator Bot" $ do
    let bot = simplifyCalculatorBot calculatorBot
    it "performs arithmetic" $ do
      let scenario =
            [mkScript|
            >>>(1 + 2)
            <<<1 + 2 = 3
            >>>(2 * 3)
            <<<2 * 3 = 6
            >>>((2 * 3) + 1)
            <<<2 * 3 + 1 = 7
            |]
      result <- runTestScript scenario $ fixBot bot mempty
      result `shouldBe` scenario

    it "can store values in state" $ do
      let scenario =
            [mkScript|
            >>>x := (1 + 2)
            <<<*variable saved*
            >>>x
            <<<"x" = 3
            |]
      result <- runTestScript scenario $ fixBot bot mempty
      result `shouldBe` scenario

sessionizedBotSpec :: Spec
sessionizedBotSpec =
  describe "Sessionized Bot" $ do
    let bot = simplifySessionBot printCalcOutput statementP $ sessionize mempty $ calculatorBot
    it "can instantiate a session" $ do
      let scenario =
            [mkScript|
            >>>new
            <<<Session Started: '0'.
            |]
      result <- runTestScript scenario $ fixBot bot mempty
      result `shouldBe` scenario

    it "can delete a session" $ do
      let scenario =
            [mkScript|
            >>>new
            <<<Session Started: '0'.
            >>>end 0
            <<<Session Ended: '0'.
            |]
      result <- runTestScript scenario $ fixBot bot mempty
      result `shouldBe` scenario

    it "preserves bot behavior" $ do
      let scenario =
            [mkScript|
            >>>new
            <<<Session Started: '0'.
            >>>use 0: (1 + 2)  
            <<<Session '0' Output:
            1 + 2 = 3
            |]
      result <- runTestScript scenario $ fixBot bot mempty
      result `shouldBe` scenario

    it "tracks multiple sessions" $ do
      let scenario =
            [mkScript|
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
      result <- runTestScript scenario $ fixBot bot mempty
      result `shouldBe` scenario
