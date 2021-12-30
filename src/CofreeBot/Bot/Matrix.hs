module CofreeBot.Bot.Matrix where

import CofreeBot.Bot ( BotAction(..), Bot(..) )
import Data.Foldable ( traverse_ )
import Data.Text qualified as T
import Network.Matrix.Client ( sendMessage, ClientSession, RoomID, TxnID(..), Event )
import System.Random ( newStdGen, randoms )

-- | A 'MatrixBot' maps from 'RoomEvent' to '[RoomEvent]'
type MatrixBot s = Bot IO s (RoomID, Event) [(RoomID, Event)]

runMatrixBot :: forall s. ClientSession -> (RoomID, Event) -> MatrixBot s -> s -> IO ()
runMatrixBot session input bot  = go
  where
  go :: s -> IO ()
  go state = do
    BotAction {..} <- runBot bot input state
    gen <- newStdGen
    let txnIds = (TxnID . T.pack . show <$> randoms @Int gen)
    traverse_ (uncurry $ sendMessage session (fst input)) $ zip (fmap snd responses) txnIds

