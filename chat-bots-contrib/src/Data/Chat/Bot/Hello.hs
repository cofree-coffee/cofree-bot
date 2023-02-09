-- | The Simplest Bot. This module serves as an introductory example
-- for bot construction.
module Data.Chat.Bot.Hello
  ( -- * Bot
    helloBot,

    -- * Serializer
    helloBotSerializer,
    helloBotParser,
  )
where

--------------------------------------------------------------------------------

import Data.Chat.Bot
import Data.Chat.Bot.Serialization
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------

-- | A pure, stateless bot which produces a 'Text' output.
helloBot :: Monad m => Bot m s () Text
helloBot = Bot $ \s () -> pure ("Are you talking to me, punk?", s)

--------------------------------------------------------------------------------

helloBotParser :: Text -> Maybe ()
helloBotParser msg =
  let name = "cofree-bot"
   in if name `Text.isInfixOf` msg
        then Just ()
        else Nothing

helloBotSerializer :: TextSerializer Text ()
helloBotSerializer = Serializer helloBotParser id
