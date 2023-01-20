{-# LANGUAGE ViewPatterns #-}

-- | A bot for general interactions with OpenAI's GPT LLM.
module CofreeBot.Bot.Behaviors.OpenAI
  ( openAIBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils.ListT (emptyListT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import OpenAI.Client qualified as OpenAI

--------------------------------------------------------------------------------

openAIBot :: OpenAI.OpenAIClient -> Bot IO () Text Text
openAIBot client =
  contraMapMaybeBot (either (const Nothing) Just . parseOnly openAIBotParser) $
    Bot $ \s (buildPrompt -> i) -> do
      liftIO (OpenAI.completeText client (OpenAI.EngineId "text-davinci-003") (i {OpenAI.tccrMaxTokens = Just 2096})) >>= \case
        Left _err -> emptyListT
        Right OpenAI.TextCompletion {tcChoices} ->
          let OpenAI.TextCompletionChoice {..} = V.head tcChoices
           in pure (T.strip tccText, s)

buildPrompt :: Text -> OpenAI.TextCompletionCreate
buildPrompt input =
  let preamble = "You are a friendly chat bot named Cofree-Bot on a server dedicated to functional programming. Please respond to the following prompt:"
   in OpenAI.defaultTextCompletionCreate $ preamble <> input

openAIBotParser :: Parser Text
openAIBotParser = do
  _ <- "chat: "
  T.pack <$> many1 anyChar
