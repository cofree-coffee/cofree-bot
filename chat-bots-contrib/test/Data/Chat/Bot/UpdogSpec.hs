{-# LANGUAGE QuasiQuotes #-}

module Data.Chat.Bot.UpdogSpec where

--------------------------------------------------------------------------------

import Data.Chat.Bot (fixBot)
import Data.Chat.Bot.Serialization qualified as S
import Data.Chat.Bot.Updog
import Scripts (mkScript)
import Test.Hspec (Spec, describe, it)
import TestServer (conformsToScript)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Updog Bot" $ do
    let bot = S.applySerializer updogBot updogSerializer
    it "responds to precisely its trigger phrases" $ do
      fixBot bot ()
        `conformsToScript` [mkScript|
        >>>whats updog
        <<<nothin much whats up with you dog
        <<<HAH GOTTEM
        >>>what snakesay
        <<<Hissss, hisssss
        <<<HAH GOTTEM
        >>>what OPP
        <<<yo, you know me!
        <<<HAH GOTTEM
        >>>WHATS updog
        <<<nothin much whats up with you dog
        <<<HAH GOTTEM
        >>>WHAT snakesay
        <<<Hissss, hisssss
        <<<HAH GOTTEM
        >>>WHAT OPP
        <<<yo, you know me!
        <<<HAH GOTTEM
        |]

    it "responds to its trigger phrases at start of message" $ do
      fixBot bot ()
        `conformsToScript` [mkScript|
        >>>whats updog cat.
        <<<nothin much whats up with you dog
        <<<HAH GOTTEM
        >>>what snakesay cat.
        <<<Hissss, hisssss
        <<<HAH GOTTEM
        >>>what OPP cat.
        <<<yo, you know me!
        <<<HAH GOTTEM
        |]
