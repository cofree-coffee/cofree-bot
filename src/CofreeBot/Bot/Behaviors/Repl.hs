module CofreeBot.Bot.Behaviors.Repl
  ( module Util
  , module GHCI
  , module MitScheme
  , module Node
  , module Python
  , replConfigs
  ) where

import CofreeBot.Bot.Behaviors.Repl.Util as Util
import CofreeBot.Bot.Behaviors.Repl.GHCI as GHCI
import CofreeBot.Bot.Behaviors.Repl.MitScheme as MitScheme
import CofreeBot.Bot.Behaviors.Repl.Node as Node
import CofreeBot.Bot.Behaviors.Repl.Python as Python
import           System.IO
import           System.Process.Typed

replConfigs :: Repls (ProcessConfig Handle Handle ())
replConfigs = Repls
  { python = pythonConfig
  , ghci = ghciConfig
  , node = nodeConfig
  , mitScheme = mitSchemeConfig 
  }
