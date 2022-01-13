module CofreeBot.Bot.Behaviors.Repl.GHCI
  ( ghciBot
  , ghciConfig
  ) where

import           CofreeBot.Bot
import           CofreeBot.Bot.Behaviors.Repl.Util
import           CofreeBot.Utils
import           Data.Profunctor
import           System.IO
import           System.Process.Typed

ghciBot' :: Process Handle Handle () -> ReplBot
ghciBot' = replBot "ghci: "

ghciBot :: Process Handle Handle () -> ReplBot
ghciBot p =
  dimap (distinguish (/= "ghci: :q")) indistinct
    $  pureStatelessBot (const $ ["I'm Sorry Dave"])
    \/ ghciBot' p

ghciConfig :: ProcessConfig Handle Handle ()
ghciConfig = replConfig "docker run -i --rm haskell 2>&1"

