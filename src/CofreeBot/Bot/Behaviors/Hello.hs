{-# OPTIONS_GHC -Wno-orphans #-}
-- | The Simplest Bot
module CofreeBot.Bot.Behaviors.Hello where

import           CofreeBot.Bot
import qualified Data.Text                     as T
import GHC.Exts
import Network.Matrix.Client

instance IsString MessageText where
  fromString msg = MessageText (T.pack msg) TextType Nothing Nothing
  
helloSimpleBot :: (Applicative m) => Bot m s T.Text [T.Text]
helloSimpleBot = pureStatelessBot $ \msg ->
  let name = "cofree-bot"
  in  if name `T.isInfixOf` msg
        then pure $ "Are you talking to me, punk?"
        else mempty

helloSimpleBot' :: (IsString (MessageContent api), MessagingAPI api, Applicative m) => Bot m s (Channel api, MessageReference api) [Action api]
helloSimpleBot' = pureStatelessBot $ \(rid, re) ->
  if messageIsMention re
    then [reply rid re "Are you talking to me, punk?"]
    else []

helloMatrixBot :: Applicative m => MatrixBot m ()
helloMatrixBot = liftSimpleBot $ helloSimpleBot
