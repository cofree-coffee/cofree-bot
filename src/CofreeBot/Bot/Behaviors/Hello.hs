-- | The Simplest Bot
module CofreeBot.Bot.Behaviors.Hello where

import           CofreeBot.Bot
import           CofreeBot.Utils.ListT          ( emptyListT )
import qualified Data.Text                     as T

helloSimpleBot :: Monad m => TextBot m s
helloSimpleBot = Bot $ \s msg ->
  let name = "cofree-bot"
  in  if name `T.isInfixOf` msg
        then pure ("Are you talking to me, punk?", s)
        else emptyListT

helloMatrixBot :: Monad m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
