-- | The Simplest Bot
module CofreeBot.Bot.Hello where

import Control.Lens hiding (to, from)
import CofreeBot.Bot
import CofreeBot.Bot.Context
import CofreeBot.Bot.Matrix
import CofreeBot.Bot.Simple
import Data.Text qualified as T
import Data.Profunctor
import Network.Matrix.Client
import Network.Matrix.Client.Lens

-- | This is the simplest example of a bot. It recieves a 'Text' and
-- sends a 'Maybe Text' and has no state.
type HelloBot m = Bot m () T.Text (Maybe T.Text)

-- | We can construct it explicitly with the 'Bot' data constructor
helloWorldBot :: Applicative m => HelloBot m
helloWorldBot = Bot $ \msg _ -> do
  let name = "cofree-bot"
  if name `T.isInfixOf` msg
     then pure $ BotAction (Just "Are you talking to me, punk?") ()
     else pure $ BotAction mempty () 

-- | We can simplify the implementation using the 'pureStatelessBot'
-- combinator.
helloWorldBot' :: Applicative m => HelloBot m
helloWorldBot' = pureStatelessBot $ \msg ->
  let name = "cofree-bot"
  in if name `T.isInfixOf` msg
     then Just "Are you talking to me, punk?"
     else mempty

-- | Lifting into a 'SimpleBot' is a good litmus test to see if you
-- have an abstration leak. In this case 'HelloBot ~ SimpleBot' so we
-- are all good.
simpleLiftHelloBot :: HelloBot IO -> SimpleBot IO ()
simpleLiftHelloBot = dimap to from
  where
    to :: T.Text -> T.Text
    to msg =  msg
    from :: Maybe T.Text -> [T.Text]
    from = \case
      Nothing -> []
      Just msg -> [msg]

-- | We can lift 'HelloBot' to operate on matrix 'Events' using 'dimap'
liftHelloBot :: Functor m => HelloBot m -> Bot m () Event [Event]
liftHelloBot = dimap to from
  where
    viewBody :: Event -> T.Text
    viewBody = (view (_EventRoomMessage . _RoomMessageText . _mtBody))

    to :: Event -> T.Text
    to = viewBody

    from :: Maybe T.Text -> [Event]
    from = maybe [] $ \msg ->
      pure (EventRoomMessage $ mkMsg msg)

    mkMsg :: T.Text -> RoomMessage
    mkMsg msg = RoomMessageText $ MessageText msg TextType Nothing Nothing

-- | We can convert 'HelloBot' into a complete 'MatrixBot'
helloMatrixBot :: MatrixBot IO ()
helloMatrixBot = mkRoomAware $ liftHelloBot helloWorldBot'
