{-# LANGUAGE QuasiQuotes #-}

module Data.Chat.Bot.HelloSpec where

--------------------------------------------------------------------------------

import Data.Chat.Bot (fixBot)
import Data.Chat.Bot.Hello
import Data.Chat.Bot.Serialization qualified as S
import Scripts (mkScript)
import Test.Hspec (Spec, describe, it)
import TestServer (conformsToScript)

--------------------------------------------------------------------------------

spec :: Spec
spec =
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
