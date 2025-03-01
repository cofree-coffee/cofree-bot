{-# LANGUAGE ViewPatterns #-}

module Data.Chat.Server.Repl
  ( -- * Text Bot
    repl,
  )
where

--------------------------------------------------------------------------------

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Machine.Moore (MooreM' (..))
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (hFlush, stdout)

--------------------------------------------------------------------------------

-- | A repl-style 'Server' for interacting with a 'Bot'.
repl :: MooreM' IO [Text] Text
repl = MooreM' $ do
  -- Read the user's input
  liftIO $ do
    putStr "<<< "
    hFlush stdout
  (Text.pack -> input) <- liftIO getLine

  pure $
    (input,) $ \outputs -> MooreM' $ do
      forM_ outputs $ \output -> do
        -- Print the bot's responses
        liftIO $ putStrLn $ Text.unpack $ ">>> " <> output

      -- Do it again
      runMooreM' repl
