module CofreeBot.Bot.Behaviors.Repl.Python
  ( pythonBot
  , pythonConfig
  ) where

import           CofreeBot.Bot.Behaviors.Repl.Util
import           System.IO
import           System.Process.Typed

pythonBot :: Process Handle Handle () -> ReplBot
pythonBot = replBot "python: "

pythonConfig :: ProcessConfig Handle Handle ()
pythonConfig = replConfig "nix-shell -p python3 --run 'python -iq' 2>&1"
