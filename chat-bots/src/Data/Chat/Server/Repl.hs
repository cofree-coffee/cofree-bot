{-# LANGUAGE ViewPatterns #-}

module Data.Chat.Server.Repl
  ( -- * Text Bot
    TextBot,
    repl,
  )
where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Monad.IO.Class
import Data.Chat.Bot
import Data.Chat.Server
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO

--------------------------------------------------------------------------------

-- | A 'TextBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type TextBot m s = Bot m s Text Text

-- | A repl-style 'Server' for interacting with a 'Bot'.
repl :: Server IO Text Text
repl = Server $ do
  -- Read the user's input
  liftIO $ do
    putStr "<<< "
    hFlush stdout
  (Text.pack -> input) <- liftIO getLine

  pure $
    (input,) $ \outputs -> Server $ do
      forM_ outputs $ \output -> do
        -- Print the bot's responses
        liftIO $ putStrLn $ Text.unpack $ ">>> " <> output

      -- Do it again
      runServer repl
