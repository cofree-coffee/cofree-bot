-- | The Simplest Bot
module CofreeBot.Bot.Behaviors.Hello (helloBot) where

import           CofreeBot.Bot
import CofreeBot.MessagingAPI
import           GHC.Exts

helloBot :: (IsString (MessageContent api), MessagingAPI api, Applicative m) => Bot m () (Channel api, MessageReference api) [APIAction api]
helloBot = pureStatelessBot $ \(rid, re) ->
  if messageMentionsBot re
    then [APIAction $ MkReply rid re "Are you talking to me, punk?"]
    else []
