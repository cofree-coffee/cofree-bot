-- | Server
module CofreeBot.Server.Type where

import CofreeBot.Bot (KBot)

type Env :: KBot
newtype Env m s o i = Env { runEnv :: s -> (i, o -> m s) }

newtype Server m o i = Server { runServer :: (i, o -> m (Server m o i)) }

fixEnv :: Functor m => Env m s o i -> s -> Server m o i
fixEnv (Env b) = go
  where
  go s = Server $ fmap (fmap (fmap go)) $ b s
