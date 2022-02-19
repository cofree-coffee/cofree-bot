{-# LANGUAGE TypeFamilyDependencies #-}
-- | Bot Messaging API
module           CofreeBot.MessagingAPI where
import qualified Data.Attoparsec.Text          as A
import           Data.Kind                      ( Type )
import qualified Data.Map.Strict               as Map

class Monad (Context api) => MessagingAPI api where
  type Context api :: Type -> Type
  -- ^ A monadic context for performing out of band effects. Eg., 'ReaderT ClientSession IO' for Matrix.
  type UserReference api :: Type
  -- ^ The identifier for the user who created the input. Eg., 'UserID' on Matrix.
  type UserMetadata api :: Type
  -- ^ Additional info about the user who created the input. Eg., 'Username' on Matrix.
  type Channel api = (r :: Type) | r -> api
  -- ^ The destination channel for the message. Eg., 'RoomID' on Matrix.
  type MessageReference api = (r :: Type) | r -> api
  -- ^ The identifier for the incoming message. Eg., 'RoomEvent' on Matrix.
  type MessageContent api :: Type
  -- ^ The message content to be sent out.

  -- NOTE: We should only use the Context to perform effects which are not observable by users.
  messageMentionsBot :: MessageReference api -> Bool
  listMembers :: Channel api -> Context api (Map.Map (UserReference api) (UserMetadata api))

  runMessageParser :: A.Parser a -> MessageReference api -> Maybe a
  parseChannel :: A.Parser (Channel api)

data Message ur um chan mr mc
  = MkMessage chan mc
  | MkReply chan mr mc
  | JoinRoom chan
  | LeaveRoom chan

newtype APIAction api = APIAction
  { unAPIAction ::
      Message
        (UserReference api)
        (UserMetadata api)
        (Channel api)
        (MessageReference api)
        (MessageContent api)
  }
