-- | The Simplest Bot
module CofreeBot.Bot.Hello where

import Control.Lens hiding (to, from)
import CofreeBot.Bot
import CofreeBot.Bot.Matrix
import CofreeBot.Bot.Simple
import Data.Text qualified as T
import Network.Matrix.Client
import Network.Matrix.Client.Lens

-- | This is the simplest example of a bot. It recieves a '(RoomID,
-- Text)' and sends a 'Maybe (RoomID, Text)' and has no state.
type HelloBot m = Bot m () (RoomID, T.Text) (Maybe (RoomID, T.Text))

-- | We can constructi it explicitly with the 'Bot' data constructor
helloWorldBot :: Applicative m => HelloBot m
helloWorldBot = Bot $ \(rid, msg) _ -> do
  let name = "cofree-bot"
  if name `T.isInfixOf` msg
     then pure $ BotAction (Just (rid, "Are you talking to me, punk?")) ()
     else pure $ BotAction Nothing () 

-- | We can simplify the implementation using the 'pureStatelessBot'
-- combinator.
helloWorldBot' :: Applicative m => HelloBot m
helloWorldBot' = pureStatelessBot $ \(rid, msg) ->
  let name = "cofree-bot"
  in if name `T.isInfixOf` msg
     then Just (rid, "Are you talking to me, punk?")
     else Nothing

-- | In order to run this bot we need to lift it into a 'Bot m ()
-- Event [Event]'
--liftHelloworldBot :: Functor m => HelloBot m -> Bot m () (RoomID, Event) [(RoomID, Event)]
liftHelloBot :: Functor m => HelloBot m -> Bot m () (RoomID, Event) [(RoomID, Event)]
liftHelloBot = dimap to from
  where
    viewBody :: Event -> T.Text
    viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

    to :: (RoomID, Event) -> (RoomID, T.Text)
    to = over _2 viewBody

    from :: Maybe (RoomID, T.Text) -> [(RoomID, Event)]
    from = maybe [] $ \(rid, msg) ->
      pure (rid, (EventRoomMessage $ mkMsg msg))

    mkMsg :: T.Text -> RoomMessage
    mkMsg msg = RoomMessageText $ MessageText msg TextType Nothing Nothing

-- | Lifting into a 'SimpleBot' is a good litmus test to see if you
-- have an abstration leak.
simpleLiftHelloBot :: HelloBot IO -> SimpleBot ()
simpleLiftHelloBot = dimap to from
  where
    to :: T.Text -> (RoomID, T.Text)
    to msg = (error "Uh oh! A Hello Bot Shouldn't need to think about RoomIDs", msg)
    from :: Maybe (RoomID, T.Text) -> [T.Text]
    from = \case
      Nothing -> []
      Just (_, msg) -> [msg]

helloMatrixBot :: MatrixBot ()
helloMatrixBot = liftHelloBot helloWorldBot'
