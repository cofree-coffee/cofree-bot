-- | The Simplest Bot. This module serves as an introductory example
-- for bot construction.
module CofreeBot.Bot.Behaviors.Hello
  ( helloSimpleBot
  , helloMatrixBot
  ) where

--------------------------------------------------------------------------------

import           CofreeBot.Bot
import qualified Data.Text                     as T

--------------------------------------------------------------------------------

-- | A pure, stateless bot which simply takes a 'T.Text' input and
-- produces a 'T.Text' output from it.
helloSimpleBot :: Applicative m => TextBot m s
helloSimpleBot = pureStatelessBot $ \msg ->
  let name = "cofree-bot"
  in  if name `T.isInfixOf` msg
        then pure "Are you talking to me, punk?"
        else mempty

-- | We can then embed our bot in the Matrix API using
-- 'liftSimpleBot'.
helloMatrixBot :: Applicative m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
