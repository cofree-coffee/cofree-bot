{-# LANGUAGE QuasiQuotes #-}

module Main where

--------------------------------------------------------------------------------

import Control.Monad.ListT (ListT)
import Data.Chat.Bot (Bot (..), fixBot)
import Data.Chat.Bot.CalculatorSpec qualified as Calculator
import Data.Chat.Bot.ContextSpec qualified as Context
import Data.Chat.Bot.HelloSpec qualified as Hello
import Data.Chat.Bot.UpdogSpec qualified as Updog
import Data.Machine.Mealy (MealyT)
import Data.Text (Text, pack)
import Scripts (Script, mkScript)
import Test.Hspec (Spec, describe, hspec, it, shouldNotBe)
import TestServer (Completion (..), conformsToScript, conformsToScript')

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  scriptedTestsSpec
  Context.spec
  Calculator.spec
  Hello.spec
  Updog.spec

-- | A test for our bot scripting tests setup.
scriptedTestsSpec :: Spec
scriptedTestsSpec = describe "Scripted tests" $ do
  -- A generic bot which returns its state back as an observation and which
  -- inverts its state on every transition.
  let myBehavior :: forall m. (Monad m) => MealyT (ListT m) Text Text
      myBehavior = flip fixBot True $ Bot $ \s _ -> return (pack $ show s, not s)

  -- This test asserts that the bot behavior conforms to the script
  it "can deal with bots that respond correctly" $ do
    myBehavior
      `conformsToScript` [mkScript|
      >>>hello
      <<<True
      >>>whatever
      <<<False
      |]

  -- This test asserts that the bot behavior does not conform to the script
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
