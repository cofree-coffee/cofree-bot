-- | The Simplest Bot. This module serves as an introductory example
-- for bot construction.
module CofreeBot.Bot.Behaviors.Hello
  ( helloSimpleBot,
    helloMatrixBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils.ListT (emptyListT)
import Data.Text qualified as T

--------------------------------------------------------------------------------

-- | A pure, stateless bot which simply takes a 'T.Text' input and
-- produces a 'T.Text' output from it.
helloSimpleBot :: Monad m => TextBot m s
helloSimpleBot = Bot $ \s msg ->
  let name = "cofree-bot"
   in if name `T.isInfixOf` msg
        then pure ("Are you talking to me, punk?", s)
        else emptyListT

-- | We can then embed our bot in the Matrix API using
-- 'embedTextBot'.
helloMatrixBot :: Monad m => MatrixBot m ()
helloMatrixBot = embedTextBot $ helloSimpleBot
