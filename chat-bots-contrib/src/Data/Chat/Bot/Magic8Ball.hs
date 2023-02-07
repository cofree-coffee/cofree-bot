module Data.Chat.Bot.Magic8Ball
  ( magic8BallBot,
    simplifyMagic8BallBot,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.ListT (emptyListT)
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Chat.Bot
import Data.Profunctor
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random

--------------------------------------------------------------------------------

magic8BallBot :: Bot IO () () Int
magic8BallBot = do
  randomRIO (1, 20)

simplifyMagic8BallBot :: forall s. Bot IO s () Int -> Bot IO s Text Text
simplifyMagic8BallBot b = do
  i <- ask
  case to i of
    Left _err -> Bot $ pure $ const emptyListT
    Right () -> dimap (const ()) from $ b
  where
    to :: Text -> Either Text ()
    to = fmap (bimap Text.pack id) $ parseOnly parseMagic8BallCommand

    from :: Int -> Text
    from i = case i `mod` 20 of
      1 -> "It is certain."
      2 -> "It is decidedly so."
      3 -> "Without a doubt."
      4 -> "Yes definitely."
      5 -> "You may rely on it."
      6 -> "As I see it, yes."
      7 -> "Most likely."
      8 -> "Outlook good."
      9 -> "Yes."
      10 -> "Signs point to yes."
      11 -> "Reply hazy, try again."
      12 -> "Ask again later."
      13 -> "Better not tell you now."
      14 -> "Cannot predict now."
      15 -> "Concentrate and ask again."
      16 -> "Don't count on it."
      17 -> "My reply is no."
      18 -> "My sources say no."
      19 -> "Outlook not so good."
      _ -> "Very doubtful."

parseMagic8BallCommand :: Parser ()
parseMagic8BallCommand = "8 ball" *> pure ()
