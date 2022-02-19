-- | The Simplest Bot
module CofreeBot.Bot.Behaviors.Hello where

import           CofreeBot.Bot
import qualified Data.Text                     as T

helloSimpleBot :: Monad m => TextBot m s
helloSimpleBot = pureStatelessBot $ \msg ->
  let name = "cofree-bot"
  in  if name `T.isInfixOf` msg
        then "Are you talking to me, punk?"
        else mempty

helloMatrixBot :: Monad m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
