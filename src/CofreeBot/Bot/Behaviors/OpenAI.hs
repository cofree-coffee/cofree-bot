{-# LANGUAGE ViewPatterns #-}

-- | A bot for general interactions with OpenAI's GPT LLM.
module CofreeBot.Bot.Behaviors.OpenAI
  ( openAIBot,
    runOpenAIBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils.ListT (emptyListT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.Trans (lift)
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import OpenAI.Client qualified as OpenAI

--------------------------------------------------------------------------------

openAIBot :: Bot (ReaderT OpenAI.OpenAIClient IO) () Text Text
openAIBot =
  contraMapMaybeBot (either (const Nothing) Just . parseOnly openAIBotParser) $
    Bot $ \() (buildPrompt -> i) -> do
      client <- lift ask
      liftIO (callOpenAI client i) >>= \case
        Left err -> liftIO (print err) >> emptyListT
        Right OpenAI.TextCompletion {tcChoices} ->
          let OpenAI.TextCompletionChoice {..} = V.head tcChoices
           in pure (T.strip tccText, ())

runOpenAIBot :: Functor m => r -> Bot (ReaderT r m) s i o -> Bot m s i o
runOpenAIBot r bot = hoistBot (`runReaderT` r) bot

callOpenAI :: OpenAI.OpenAIClient -> OpenAI.TextCompletionCreate -> IO (Either OpenAI.ClientError OpenAI.TextCompletion)
callOpenAI client i = OpenAI.completeText client (OpenAI.EngineId "text-davinci-003") (i {OpenAI.tccrMaxTokens = Just 2096})

buildPrompt :: Text -> OpenAI.TextCompletionCreate
buildPrompt input =
  let preamble = "You are a friendly chat bot named Cofree-Bot on a server dedicated to functional programming. Please respond to the following prompt:"
   in OpenAI.defaultTextCompletionCreate $ preamble <> input

openAIBotParser :: Parser Text
openAIBotParser = do
  _ <- "chat: "
  T.pack <$> many1 anyChar
