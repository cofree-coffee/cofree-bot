module CofreeBot.Bot.Behaviors.CoinFlip where

import           CofreeBot.Bot
import           CofreeBot.Utils.ListT
import           Data.Attoparsec.Text
import           Data.Bifunctor                 ( bimap )
import qualified Data.Text                     as T
import           System.Random

coinFlipBot :: Bot IO () () Bool
coinFlipBot = Bot $ \_ s -> ListT $ do
  gen <- newStdGen
  let (result, _) = random @Bool gen
  pure $ ConsF (BotAction result s) emptyListT

simplifyCoinFlipBot :: forall s . Bot IO s () Bool -> TextBot IO s
simplifyCoinFlipBot (Bot bot) = Bot $ \i s -> case to i of
  Left  _ -> pure $ BotAction mempty s
  Right _ -> fmap (fmap from) $ bot () s
 where
  to :: T.Text -> Either T.Text ()
  to = fmap (bimap T.pack id) $ parseOnly parseCoinFlipCommand

  from :: Bool -> T.Text
  from = \case
    True  -> "Coin Flip Result: True"
    False -> "Coin Flip Result: False"

parseCoinFlipCommand :: Parser ()
parseCoinFlipCommand = "flip a coin" *> pure ()
