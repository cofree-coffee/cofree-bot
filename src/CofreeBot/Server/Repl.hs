{-# LANGUAGE ViewPatterns #-}
-- | Repl Server
module CofreeBot.Server.Repl where

import           CofreeBot.Bot                  ( Behavior(..)
                                                , Bot(..)
                                                , BotAction(..)
                                                , Fix(..)
                                                )
import           CofreeBot.Server.Type
import           Data.Foldable                  ( traverse_ )
import qualified Data.Text                     as T
import           System.IO                      ( hFlush
                                                , stdout
                                                )

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type TextBot m s = Bot m s T.Text [T.Text]

repl :: IO (Server IO [T.Text] T.Text)
repl = do
  -- Read the user's input
  putStr "<<< "
  hFlush stdout
  (T.pack -> input) <- getLine

  return $ Server $ (input, ) $ \outputs -> do
    -- Print the bot's responses
    traverse_ (putStrLn . T.unpack . (">>> " <>)) outputs

    -- Do it again
    repl

annihilate :: Monad m => Behavior m i o -> Server m o i -> Fix m
annihilate (Behavior a) (Server (i, b)) = Fix $ do
  BotAction { nextState = a', responses = o } <- a i
  b' <- b o
  return $ annihilate a' b'

