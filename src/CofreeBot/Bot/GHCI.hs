module CofreeBot.Bot.GHCI where

import Control.Lens hiding (to, from)
import CofreeBot.Bot
import CofreeBot.Bot.Context
import CofreeBot.Bot.Matrix
import CofreeBot.Bot.Simple
import Data.Text qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens

type GhciBot = Bot IO () T.Text (Maybe T.Text)
