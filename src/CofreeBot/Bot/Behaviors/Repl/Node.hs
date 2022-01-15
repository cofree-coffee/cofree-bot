module CofreeBot.Bot.Behaviors.Repl.Node
  ( nodeBot
  , nodeConfig
  ) where

import           CofreeBot.Bot.Behaviors.Repl.Util
import           System.IO
import           System.Process.Typed

nodeBot :: Process Handle Handle () -> ReplBot
nodeBot = replBot "node: "

nodeConfig :: ProcessConfig Handle Handle ()
nodeConfig = replConfig "nix-shell -p nodejs --run 'node -i' 2>&1"
