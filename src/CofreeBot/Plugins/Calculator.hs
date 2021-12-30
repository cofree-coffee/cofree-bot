{-# LANGUAGE TypeOperators #-}
module CofreeBot.Plugins.Calculator where

import CofreeBot.Bot
import CofreeBot.Plugins.Calculator.Language
import Data.Foldable
import Data.Profunctor
import Data.Text qualified as T
import System.IO (stdout, hFlush)
import Data.Functor

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

nudge :: Applicative m => Bot m s i o \/ Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudge = either
  (\(Bot b) ->
    Bot $ either
      ((fmap . fmap . fmap . fmap) (Just . Left) $ b)
      (const $ \s -> pure $ BotAction Nothing s))
  (\(Bot b) ->
    Bot $ either
      (const $ \s -> pure $ BotAction Nothing s)
      ((fmap . fmap . fmap . fmap) (Just . Right) $ b))

nudgeLeft :: Applicative m => Bot m s i o -> Bot m s (i \/ i') (o \?/ o')
nudgeLeft = nudge . Left

nudgeRight :: Applicative m => Bot m s i' o' -> Bot m s (i \/ i') (o \?/ o')
nudgeRight = nudge . Right

(\/) :: Functor m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) (Bot b1) (Bot b2) = Bot $ either
  ((fmap . fmap . fmap) Left . b1)
  ((fmap . fmap . fmap) Right . b2)

same :: Either x x -> x
same = either id id

pureStatelessBot :: Applicative m => (i -> o) -> Bot m s i o
pureStatelessBot f = Bot $ \i s -> pure $ BotAction (f i) s

--------------------------------------------------------------------------------
-- Calculator bot
--------------------------------------------------------------------------------

type CalculatorBot = Bot IO CalcState Program (Either CalcError [CalcResp])

calculatorBot :: CalculatorBot
calculatorBot = Bot $ \program state ->
  fmap (uncurry BotAction) $ interpretProgram'' program state

parseErrorBot :: Applicative m => Bot m s ParseError T.Text
parseErrorBot = pureStatelessBot $ \ParseError {..} ->
  "Failed to parse msg: \"" <> parseInput <> "\". Error message was: \"" <> parseError <> "\"."

simpleCalculatorBot :: SimpleBot CalcState
simpleCalculatorBot
  = dimap parseProgram same
  $ rmap (:[]) parseErrorBot \/ rmap printTxt calculatorBot
  where
  printTxt :: Either CalcError [CalcResp] -> [T.Text]
  printTxt = \case
    Left err -> pure $ T.pack $ show err
    Right resps -> resps <&> \case
      Log e n -> T.pack $ show e <> " = " <> show n

runSimpleBot :: forall s. SimpleBot s -> s -> IO ()
runSimpleBot bot = go
  where
  go :: s -> IO ()
  go state = do
    putStr "<<< "
    hFlush stdout
    input <- getLine
    BotAction {..} <- runBot bot (T.pack input) state
    traverse_ (putStrLn . T.unpack . (">>> " <>)) responses
    go nextState
