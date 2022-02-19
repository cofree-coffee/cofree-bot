module CofreeBot.Bot.Behaviors.Admin where

import           CofreeBot.Bot
import           CofreeBot.MessagingAPI
import           CofreeBot.Utils
import           Control.Applicative
import           Control.Lens            hiding ( Context
                                                , re
                                                , to
                                                )
import qualified Data.Attoparsec.Text    as A

adminBot :: forall api m. (MessagingAPI api, Applicative m) => Bot m () (Channel api, MessageReference api) [APIAction api]
adminBot = mapMaybeBot toAdminAction $ rmap pure joinOrLeaveRoom
  where
    toAdminAction :: (Channel api, MessageReference api) -> Maybe (AdminAction api)
    toAdminAction (_chan, mr) = runMessageParser adminActionParser mr

joinOrLeaveRoom
  :: (MessagingAPI api, Applicative m) => Bot m () (AdminAction api) (APIAction api)
joinOrLeaveRoom = dimap adminActionInput indistinct $ joinRoom \/ leaveRoom
  where
    adminActionInput :: AdminAction api -> Channel api \/ Channel api
    adminActionInput = \case
      AAJoinRoom chan -> Left chan
      AALeaveRoom chan -> Right chan

joinRoom
  :: (MessagingAPI api, Applicative m) => Bot m () (Channel api) (APIAction api)
joinRoom = Bot $ \channel s -> pure $ BotAction (APIAction $ JoinRoom channel) s

leaveRoom
  :: (MessagingAPI api, Applicative m) => Bot m () (Channel api) (APIAction api)
leaveRoom = Bot $ \channel s -> pure $ BotAction (APIAction $ LeaveRoom channel) s

data AdminAction api
  = AAJoinRoom (Channel api)
  | AALeaveRoom (Channel api)
--  | AAListRooms

adminActionParser :: MessagingAPI api => A.Parser (AdminAction api)
adminActionParser =
  let -- listRoomsP = AAListRooms <$ ("!List Rooms" *> _)
      joinRoomP  = AAJoinRoom <$> ("!Join " *> parseChannel)
      leaveRoomP = AALeaveRoom <$> ("!Leave " *> parseChannel)
  in  joinRoomP <|> leaveRoomP -- <|> listRoomsP
