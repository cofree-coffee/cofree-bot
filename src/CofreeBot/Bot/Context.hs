{-# LANGUAGE MultiParamTypeClasses #-}
-- | Context Transformations for bots
module CofreeBot.Bot.Context where

import CofreeBot.Bot
import Network.Matrix.Client

type RoomAware :: KBot -> KBot
type RoomAware bot m s i o = bot m s (RoomID, i) (RoomID, o)

-- | 'mkRoomAware' makes a bot "room aware"
mkRoomAware :: Applicative m => Bot m s i o -> RoomAware Bot m s i o
mkRoomAware = threadFirst
