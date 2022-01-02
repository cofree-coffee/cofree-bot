module CofreeBot.Bot.Matrix where

import Control.Lens qualified as L
import CofreeBot.Bot ( BotAction(..), Bot(..) )
import Data.Foldable
import Data.Text qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens
import System.Random ( newStdGen, randoms )

-- | A 'MatrixBot' maps from 'RoomEvent' to '[RoomEvent]'
type MatrixBot m s = Bot m s (RoomID, Event) (RoomID, [Event])

runMatrixBot :: forall s. ClientSession -> (RoomID, Event) -> MatrixBot IO s -> s -> IO ()
runMatrixBot session input bot  = go
  where
  go :: s -> IO ()
  go state = do
    BotAction {..} <- runBot bot input state
    gen <- newStdGen
    let txnIds = (TxnID . T.pack . show <$> randoms @Int gen)
    traverse_ (uncurry $ sendMessage session (fst input)) $ zip (snd responses) txnIds

liftMaybeTextToEvent :: Functor f => Bot f s T.Text (Maybe T.Text) -> Bot f s Event [Event]
liftMaybeTextToEvent (Bot bot) = Bot $ \i s -> fmap from $ bot (to i) s
  where
    to :: Event -> T.Text
    to = L.view (_EventRoomMessage . _RoomMessageText . _mtBody)

    from :: BotAction s (Maybe T.Text) -> BotAction s [Event]
    from x = fmap (fmap mkMsg . toList) x

    mkMsg :: T.Text -> Event
    mkMsg msg = EventRoomMessage $ RoomMessageText $ MessageText msg TextType Nothing Nothing
