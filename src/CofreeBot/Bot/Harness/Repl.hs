{-# LANGUAGE EmptyCase #-}
-- | Repl Harness
module CofreeBot.Bot.Harness.Repl (runRepl) where

import           CofreeBot.Bot
import           CofreeBot.MessagingAPI
import           Data.Foldable
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import           Data.Profunctor
import           Data.Proxy
import qualified Data.Text                     as T
import           Data.Void
import           System.IO

data Repl

-- TODO(SOLOMON): Think through correct associated types for 'Repl'
instance MessagingAPI Repl where
  type Context Repl = Proxy
  type UserReference Repl = Void
  type UserMetadata Repl = Void
  type Channel Repl = ()
  type MessageReference Repl = T.Text
  type MessageContent Repl = T.Text

  listMembers :: () -> Proxy (Map.Map Void Void)
  listMembers _  = Proxy

  messageMentionsBot :: T.Text -> Bool
  messageMentionsBot msg = "cofree-bot" `T.isInfixOf` msg

  runMessageParser :: A.Parser a -> T.Text -> Maybe a
  runMessageParser p msg = either (const Nothing) Just $ A.parseOnly p msg

  parseChannel :: A.Parser ()
  parseChannel = pure ()

prettyPrint :: APIAction Repl -> T.Text
prettyPrint (APIAction (MkMessage _ msg)) = msg
prettyPrint (APIAction (MkReply _ _ msg)) = msg
prettyPrint (APIAction (JoinRoom _)) = "There are no channels to join."
prettyPrint (APIAction (LeaveRoom _)) = "There are no channels to leave."

-- | An evaluator for running bots in the repl
runRepl :: forall s. Bot IO s (Channel Repl, MessageReference Repl) [APIAction Repl] -> s -> IO ()
runRepl bot = go
  where
    go :: s -> IO ()
    go state = do
      putStr "<<< "
      hFlush stdout
      input          <- getLine
      BotAction {..} <- runBot (lmap ((),) bot) (T.pack input) state
      traverse_ (putStrLn . T.unpack . (">>> " <>)) (fmap prettyPrint responses)
      go nextState
