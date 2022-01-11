module CofreeBot.Bot.CoinFlip where

import           CofreeBot.Bot
import           CofreeBot.Bot.Simple
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( bimap )
import qualified Data.Text                     as T
import           System.Random

coinFlipBot :: Bot IO () () Bool
coinFlipBot = Bot $ \_ s -> do
  gen <- newStdGen
  let (result, _) = _ gen
  pure $ BotAction result s

simplifyCoinFlipBot :: forall s . Bot IO s () Bool -> TextBot IO s
simplifyCoinFlipBot (Bot bot) = Bot $ \i s -> case to i of
  Left  _ -> pure $ BotAction [] s
  Right _ -> fmap (fmap from) $ bot () s
 where
  to :: T.Text -> Either T.Text ()
  to = fmap (bimap T.pack id) $ parseOnly parseCoinFlipCommand

  from :: Bool -> [T.Text]
  from = \case
    True  -> pure "Coin Flip Result: True"
    False -> pure "Coin Flip Result: False"

parseCoinFlipCommand :: Parser ()
parseCoinFlipCommand = "flip a coin" *> pure ()
