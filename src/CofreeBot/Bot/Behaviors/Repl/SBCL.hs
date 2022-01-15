module CofreeBot.Bot.Behaviors.Repl.SBCL
  ( sbclBot
  , sbclConfig
  ) where

import           CofreeBot.Bot.Behaviors.Repl.Util
import           System.IO
import           System.Process.Typed

sbclBot :: Process Handle Handle () -> ReplBot
sbclBot = replBot "sbcl: "

sbclConfig :: ProcessConfig Handle Handle ()
sbclConfig = replConfig "nix-shell -p sbcl --run sbcl 2>&1"
