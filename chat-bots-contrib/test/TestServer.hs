module TestServer
  ( conformsToScript',
    conformsToScript,
    Completion (..),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Except
  ( MonadError (..),
    liftEither,
    runExceptT,
  )
import Control.Monad.ListT (ListT, hoistListT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Chat.Server
  ( Env (..),
    annihilate,
    fixEnv,
  )
import Data.Fix (Fix (..))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Machine.Mealy (MealyT, hoistMealyT)
import Data.Machine.Moore (MooreT)
import Data.Text (Text)
import Data.Void
import Scripts
import Test.Hspec (shouldBe)

--------------------------------------------------------------------------------

data Completion i o
  = Passed
  | Failed (FailedCompletion i o)
  deriving (Show, Eq)

data FailedCompletion i o = FailedCompletion
  { problematicInput :: i,
    expected :: [o],
    actual :: [o],
    remainder :: [Interaction i o]
  }
  deriving (Show, Eq)

type ReplayServerState i o = Either (Completion i o) (NonEmpty (Interaction i o))

initReplayServerState :: Script -> ReplayServerState Text Text
initReplayServerState (Script interactions) = case interactions of
  [] -> Left Passed
  x : xs -> Right $ x :| xs

replayServer ::
  (Eq o, MonadError (Completion i o) m) =>
  ReplayServerState i o ->
  MooreT m [o] i
replayServer = fixEnv $ Env $ (liftEither .) $ \case
  Left completion -> Left completion
  Right (Interaction prompt expectedResponses :| rest) -> Right $ (prompt,) $ \actualResponses ->
    if actualResponses == reverse expectedResponses
      then maybe (Left Passed) Right $ nonEmpty rest
      else
        Left $
          Failed
            FailedCompletion
              { problematicInput = prompt,
                expected = expectedResponses,
                actual = actualResponses,
                remainder = rest
              }

conformsToScript' :: MealyT (ListT IO) Text Text -> Script -> IO (Completion Text Text)
conformsToScript' behavior script = do
  let server = replayServer (initReplayServerState script)
  fmap onlyLeft $ runExceptT $ bindFix $ annihilate server (hoistMealyT (hoistListT lift) behavior)
  where
    -- TODO: move these somewhere else
    bindFix :: (Monad m) => Fix m -> m Void
    bindFix (Fix m) = m >>= bindFix

    onlyLeft :: Either a Void -> a
    onlyLeft = \case
      Left x -> x
      Right v -> absurd v

conformsToScript :: MealyT (ListT IO) Text Text -> Script -> IO ()
conformsToScript behavior script = do
  result <- behavior `conformsToScript'` script
  result `shouldBe` Passed
