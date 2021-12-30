module CofreeBot.Bot.Simple where

import CofreeBot.Bot ( BotAction(..), Bot(..) )
import CofreeBot.Bot.Matrix ( MatrixBot )
import Data.Foldable ( traverse_ )
import Data.Profunctor ( dimap )
import Data.Text qualified as T
import Network.Matrix.Client ( RoomID, Event )
import System.IO ( stdout, hFlush )

-- | A 'SimpleBot' maps from 'Text' to '[Text]'. Lifting into a
-- 'SimpleBot' is useful for locally debugging another bot.
type SimpleBot s = Bot IO s T.Text [T.Text]

-- | Lift a 'SimpleBot' into a 'MatrixBot'
liftSimpleBot :: ((RoomID, Event) -> T.Text) -> (T.Text -> (RoomID, Event)) -> SimpleBot s -> MatrixBot s
liftSimpleBot to from = dimap to (fmap from)

-- | An evaluator for running 'SimpleBots' in 'IO'
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
