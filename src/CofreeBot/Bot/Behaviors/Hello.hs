-- | The Simplest Bot
module CofreeBot.Bot.Behaviors.Hello where

import           CofreeBot.Bot
import           CofreeBot.Server
import qualified Data.Text                     as T

helloSimpleBot :: Applicative m => TextBot m s
helloSimpleBot = pureStatelessBot $ \msg ->
  let name = "cofree-bot"
  in  if name `T.isInfixOf` msg
        then pure "Are you talking to me, punk?"
        else mempty

helloMatrixBot :: Applicative m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
