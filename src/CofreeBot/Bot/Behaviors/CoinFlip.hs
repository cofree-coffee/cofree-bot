module CofreeBot.Bot.Behaviors.CoinFlip where

import           CofreeBot.Bot
import           Control.Monad.Reader
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( bimap )
import           Data.Profunctor
import qualified Data.Text                     as T
import           System.Random

coinFlipBot :: Bot IO () () Bool
coinFlipBot = do
  randomIO

simplifyCoinFlipBot :: forall s . Bot IO s () Bool -> TextBot IO s
simplifyCoinFlipBot b = do
  t <- ask
  case to t of
    Left  _ -> pure []
    Right _ -> dimap (const ()) from $ b
 where
  to :: T.Text -> Either T.Text ()
  to = fmap (bimap T.pack id) $ parseOnly parseCoinFlipCommand

  from :: Bool -> [T.Text]
  from = \case
    True  -> pure "Coin Flip Result: True"
    False -> pure "Coin Flip Result: False"

parseCoinFlipCommand :: Parser ()
parseCoinFlipCommand = "flip a coin" *> pure ()
