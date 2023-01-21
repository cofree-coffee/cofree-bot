{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | A bot for general interactions with OpenAI's GPT LLM.
module CofreeBot.Bot.Behaviors.OpenAI
  ( openAIBot,
    runOpenAIBot,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
import CofreeBot.Utils ((...))
import CofreeBot.Utils.ListT (emptyListT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.Trans (lift)
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import OpenAI.Client qualified as OpenAI
import Text.QuasiText qualified as QT

--------------------------------------------------------------------------------

data Interaction = Interaction {prompt :: Text, completion :: Text}
  deriving (Show, Read)

prettyInteraction :: Interaction -> Text
prettyInteraction Interaction {..} =
  [QT.embed|
User: $prompt
Cofree-bot: $completion

|]

prettyHistory :: [Interaction] -> Text
prettyHistory = foldMap prettyInteraction

openAIBot :: Bot (ReaderT OpenAI.OpenAIClient IO) [Interaction] Text Text
openAIBot =
  contraMapMaybeBot (either (const Nothing) Just . parseOnly openAIBotParser) $
    Bot $ \history i -> do
      let prompt = buildPrompt i history
      client <- lift ask
      liftIO (callOpenAI client prompt) >>= \case
        Left err -> liftIO (print err) >> emptyListT
        Right OpenAI.TextCompletion {tcChoices} ->
          let OpenAI.TextCompletionChoice {..} = V.head tcChoices
              response = T.strip tccText
           in pure (response, Interaction i response : history)

runOpenAIBot :: Functor m => r -> Bot (ReaderT r m) s i o -> Bot m s i o
runOpenAIBot r bot = hoistBot (`runReaderT` r) bot

callOpenAI :: OpenAI.OpenAIClient -> OpenAI.TextCompletionCreate -> IO (Either OpenAI.ClientError OpenAI.TextCompletion)
callOpenAI client i = OpenAI.completeText client (OpenAI.EngineId "text-davinci-003") (i {OpenAI.tccrMaxTokens = Just 2096})

preamble :: Text -> [Interaction] -> Text
preamble prompt (prettyHistory -> history) =
  [QT.embed|
Chat History:
```
$history
```

You are a friendly chat bot named Cofree-Bot on a server dedicated to functional programming.
Given the Chat History defined above, please respond to the following prompt, but do not prepend your statmements with your name:

```
$prompt
```
|]

buildPrompt :: Text -> [Interaction] -> OpenAI.TextCompletionCreate
buildPrompt = OpenAI.defaultTextCompletionCreate ... preamble

openAIBotParser :: Parser Text
openAIBotParser = do
  _ <- "chat: "
  T.pack <$> many1 anyChar
