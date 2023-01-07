{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TestServer
  ( conformsToScript',
    conformsToScript,
    Completion (..),
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
  ( Behavior (Behavior),
    Env (..),
    Server (..),
    annihilate,
    fixEnv,
    hoistBehavior,
    hoistBot,
    hoistServer,
    liftBehavior,
    loop,
  )
import CofreeBot.Utils.ListT
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    MonadTrans (..),
    liftEither,
    runExceptT,
  )
import Control.Monad.IO.Class
import Control.Monad.State
  ( MonadState,
    StateT (..),
    gets,
    modify,
  )
import Data.Fix (Fix (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Text (Text)
import Data.Void
import Scripts
import Test.Hspec (shouldBe)

--------------------------------------------------------------------------------

data Completion i o
  = Passed
  | Failed {problematicInput :: i, expected :: [o], actual :: [o], remainder :: [Interaction i o]}
  deriving (Show, Eq)

type ReplayServerState i o = Either (Completion i o) (NonEmpty (Interaction i o))

initReplayServerState :: Script -> ReplayServerState Text Text
initReplayServerState (Script interactions) = case interactions of
  [] -> Left Passed
  x : xs -> Right $ x :| xs

replayServer ::
  (Eq o, MonadError (Completion i o) m) =>
  ReplayServerState i o ->
  Server m o i
replayServer = fixEnv $ Env $ (liftEither .) $ \case
  Left completion -> Left completion
  Right (Interaction prompt expectedResponses :| rest) -> Right $ (prompt,) $ \actualResponses ->
    if actualResponses == expectedResponses
      then maybe (Left Passed) Right $ nonEmpty rest
      else
        Left $
          Failed
            { problematicInput = prompt,
              expected = expectedResponses,
              actual = actualResponses,
              remainder = rest
            }

conformsToScript' :: Behavior IO Text Text -> Script -> IO (Completion Text Text)
conformsToScript' behavior script = do
  let server = replayServer (initReplayServerState script)
  fmap onlyLeft $ runExceptT $ bindFix $ annihilate server (hoistBehavior lift behavior)
  where
    -- TODO: move these somewhere else
    bindFix :: Monad m => Fix m -> m Void
    bindFix (Fix m) = m >>= bindFix

    onlyLeft :: Either a Void -> a
    onlyLeft = \case
      Left x -> x
      Right v -> absurd v

conformsToScript :: Behavior IO Text Text -> Script -> IO ()
conformsToScript behavior script = do
  result <- behavior `conformsToScript'` script
  result `shouldBe` Passed
