{-# LANGUAGE QuasiQuotes #-}

module Main where

--------------------------------------------------------------------------------

import CofreeBot.Bot (fixBot)
import CofreeBot.Bot.Behaviors.Calculator
  ( calculatorBot,
    simplifyCalculatorBot,
  )
import CofreeBot.Bot.Behaviors.Hello (helloSimpleBot)
import Scripts (mkScript)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import TestServer (runTestScript)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  helloBotSpec
  calculatorBotSpec

helloBotSpec :: Spec
helloBotSpec =
  describe "Hello Bot" $ do
    it "responds to precisely its trigger phrase" $ do
      let scenario =
            [mkScript|
            >>>cofree-bot
            <<<Are you talking to me, punk?
            |]
      result <- runTestScript scenario $ fixBot helloSimpleBot ()
      result `shouldBe` scenario

    it "responds to its trigger phrase embedded in a sentence" $ do
      let scenario =
            [mkScript|
            >>>hows it going cofree-bot
            <<<Are you talking to me, punk?
            |]
      result <- runTestScript scenario $ fixBot helloSimpleBot ()
      result `shouldBe` scenario

calculatorBotSpec :: Spec
calculatorBotSpec =
  describe "Calculator Bot" $ do
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
      result <- runTestScript scenario $ fixBot (simplifyCalculatorBot calculatorBot) mempty
      result `shouldBe` scenario

    it "can store values in state" $ do
      let scenario =
            [mkScript|
            >>>x := (1 + 2)
            <<<*variable saved*
            >>>x
            <<<"x" = 3
            |]
      result <- runTestScript scenario $ fixBot (simplifyCalculatorBot calculatorBot) mempty
      result `shouldBe` scenario
