-- | The Simplest Bot. This module serves as an introductory example
-- for bot construction.
module Data.Chat.Bot.Hello
  ( helloSimpleBot,
    helloMatrixBot,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.ListT (emptyListT)
import Data.Chat.Bot
import Data.Chat.Server.Matrix
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

-- | A pure, stateless bot which simply takes a 'Text' input and
-- produces a 'Text' output from it.
helloSimpleBot :: Monad m => Bot m s Text Text
helloSimpleBot = Bot $ \s msg ->
  let name = "cofree-bot"
   in if name `Text.isInfixOf` msg
        then pure ("Are you talking to me, punk?", s)
        else emptyListT

-- | We can then embed our bot in the Matrix API using
-- 'liftSimpleBot'.
helloMatrixBot :: Monad m => Bot m () (RoomID, Event) (RoomID, Event)
helloMatrixBot = embedTextBot helloSimpleBot
