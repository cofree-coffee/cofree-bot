{-# LANGUAGE QuasiQuotes #-}

module Data.Chat.Bot.QuoteSpec where

--------------------------------------------------------------------------------

import Data.Chat.Bot (fixBot)
import Data.Chat.Bot.Quote
import Data.Chat.Bot.Serialization qualified as S
import Scripts (mkScript)
import Test.Hspec (Spec, describe, it)
import TestServer (conformsToScript)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Quote Bot" $ do
    let bot = S.applySerializer quoteBot quoteBotSerializer
    it "Empty quote archive lookup failure" $ do
      fixBot bot []
        `conformsToScript` [mkScript|
        >>>!q
        <<<I'm sorry there are no quotes
        >>>!quote
        <<<I'm sorry there are no quotes
        |]

    it "Add some quotes then look them up" $ do
      fixBot bot []
        `conformsToScript` [mkScript|
        >>>!qsave helloworld
        <<<Quote recorded!
        >>>!q 0
        <<<"helloworld"
        >>>!q
        <<<"helloworld"
        >>>!qs goodbye moon
        <<<Quote recorded!
        >>>!q 1
        <<<"goodbye moon"
        |]

    it "Can delete quotes" $ do
      fixBot bot []
        `conformsToScript` [mkScript|
        >>>!qsave helloworld
        <<<Quote recorded!
        >>>!q 0
        <<<"helloworld"
        >>>!qdelete 0
        <<<Deleted quote at index 0
        >>>!quote
        <<<I'm sorry there are no quotes
        >>>!qs helloworld
        <<<Quote recorded!
        >>>!q 0
        <<<"helloworld"
        >>>!qd 0
        <<<Deleted quote at index 0
        >>>!q
        <<<I'm sorry there are no quotes
        |]
