{-# LANGUAGE CPP #-}

module Data.Chat.Bot.Quote
  ( -- * Bot
    quoteBot,
    Command (..),

    -- * Serializer
    quoteBotSerializer,
    quoteBotParser,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Alternative (..), optional)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Attoparsec.Text qualified as Atto
import Data.Chat.Bot
import Data.Chat.Bot.Serialization
#if MIN_VERSION_base(4,19,0)
import Data.List ((!?))
#endif
import Data.Text (Text)
import Data.Text.Display
import System.Random (randomRIO)

--------------------------------------------------------------------------------

data Command = Save Text | Lookup Int | Random | Delete Int

-- | A pure, stateless bot which produces a 'Text' output.
quoteBot :: (MonadIO m) => Bot m [Text] Command Text
quoteBot = Bot $ \qs -> \case
  Save q -> pure ("Quote recorded!", qs <> [q])
  Lookup n ->
    case qs !? n of
      Nothing -> pure ("No quote exists with that index :(", qs)
      Just q -> pure ("\"" <> q <> "\"", qs)
  Random
    | null qs ->
        pure ("I'm sorry there are no quotes", qs)
  Random -> do
    n <- liftIO $ randomRIO (0, length qs - 1)
    pure ("\"" <> qs !! n <> "\"", qs)
  Delete n | length qs > n -> do
    let (xs, ys) = splitAt n qs
    pure ("Deleted quote at index " <> display n, xs <> drop 1 ys)
  Delete _ -> do
    pure ("Index out of range!", qs)

#if !MIN_VERSION_base(4,19,0)
(!?) :: [a] -> Int -> Maybe a
{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
#endif

--------------------------------------------------------------------------------

quoteBotParser :: Text -> Maybe Command
quoteBotParser = either (const Nothing) Just . Atto.parseOnly parser
  where
    parser = do
      void "!q"
      save <|> delete <|> lookup <|> rando
      where
        rando = do
          void $ optional "uote"
          Atto.skipSpace
          pure Random
        lookup = do
          void $ optional "uote"
          Atto.skipSpace
          fmap (Lookup . read @Int) $ some Atto.digit
        save = do
          void $ "save" <|> "s"
          Atto.skipSpace
          Save <$> Atto.takeText
        delete = do
          void $ "delete" <|> "d"
          Atto.skipSpace
          n <- read @Int <$> Atto.many1 Atto.digit
          pure $ Delete n

quoteBotSerializer :: TextSerializer Text Command
quoteBotSerializer = Serializer quoteBotParser id
