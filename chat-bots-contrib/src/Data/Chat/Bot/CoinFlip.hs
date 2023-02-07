module Data.Chat.Bot.CoinFlip where

--------------------------------------------------------------------------------

import Control.Monad.ListT (emptyListT)
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Chat.Bot
import Data.Profunctor
import Data.Text (Text)
import Data.Text qualified as Text
import System.Random (randomIO)

--------------------------------------------------------------------------------

coinFlipBot :: Bot IO () () Bool
coinFlipBot = do
  randomIO

simplifyCoinFlipBot :: forall s. Bot IO s () Bool -> Bot IO s Text Text
simplifyCoinFlipBot b = do
  t <- ask
  case to t of
    Left _err -> Bot $ pure $ const emptyListT
    Right _ -> dimap (const ()) from $ b
  where
    to :: Text -> Either Text ()
    to = fmap (bimap Text.pack id) $ parseOnly parseCoinFlipCommand

    from :: Bool -> Text
    from = \case
      True -> "Coin Flip Result: True"
      False -> "Coin Flip Result: False"

parseCoinFlipCommand :: Parser ()
parseCoinFlipCommand = "flip a coin" *> pure ()
