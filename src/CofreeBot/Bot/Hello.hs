-- | The Simplest Bot
module CofreeBot.Bot.Hello where

import CofreeBot.Bot
import CofreeBot.Bot.Matrix
import CofreeBot.Bot.Simple
import Data.Text qualified as T

helloSimpleBot :: Applicative m => TextBot m s
helloSimpleBot = pureStatelessBot $ \msg ->
  let name = "cofree-bot"
  in if name `T.isInfixOf` msg
     then pure "Are you talking to me, punk?"
     else mempty

helloMatrixBot :: Applicative m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
