module CofreeBot.Bot.Behaviors.Repl.MitScheme
  ( mitSchemeBot
  , mitSchemeConfig
  ) where

import           CofreeBot.Bot.Behaviors.Repl.Util
import           System.IO
import           System.Process.Typed

mitSchemeBot :: Process Handle Handle () -> ReplBot
mitSchemeBot = replBot "mit-scheme: "

mitSchemeConfig :: ProcessConfig Handle Handle ()
mitSchemeConfig = replConfig "nix-shell -p mitscheme --run 'mit-scheme' 2>&1"
