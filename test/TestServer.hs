module TestServer
  ( runTestScript,
  )
where

--------------------------------------------------------------------------------

import CofreeBot.Bot
  ( Behavior (Behavior),
    Server (..),
    liftBehavior,
    loop,
  )
import CofreeBot.Utils.ListT
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    MonadTrans (..),
    runExceptT,
  )
import Control.Monad.State
  ( MonadState,
    StateT (..),
    gets,
    modify,
  )
import Data.Fix (Fix (..))
import Data.Text (Text)
import Scripts

--------------------------------------------------------------------------------

nextInput :: Monad m => StateT ([i], [Interaction i o]) m (Maybe i)
nextInput =
  gets fst >>= \case
    [] -> pure Nothing
    (i : xs) -> do
      modify $ \(_, os) -> (xs, os)
      pure (Just i)

logResult :: Monad m => i -> [o] -> StateT ([i], [Interaction i o]) m ()
logResult i os = modify $ \(inputs, results) -> (inputs, (results <> [Interaction i os]))

-- | A 'Server' which feeds a pre-programed series of inputs into
-- its paired bot.
testServer :: Monad m => Server (StateT ([i], [Interaction i o]) m) o (Maybe i)
testServer =
  Server $ do
    nextInput >>= \case
      Nothing -> pure $ (Nothing, const $ Server $ runServer $ testServer)
      Just input -> do
        pure $ (Just input,) $ \os -> Server $ do
          logResult input os
          runServer $ testServer

type MaybeT m = ExceptT () m

boundedAnnihilation ::
  MonadState (([i], [Interaction i o])) m =>
  Server m o (Maybe i) ->
  Behavior m i o ->
  Fix (MaybeT m)
boundedAnnihilation (Server server) b@(Behavior botBehavior) = Fix $ do
  lift server >>= \case
    (Nothing, _nextServer) -> throwError ()
    (Just i, nextServer) -> do
      xs <- lift $ fromListT $ botBehavior i
      let o = fmap fst xs
          server' = nextServer o
      pure $ boundedAnnihilation server' $ case xs of
        [] -> b
        _ -> snd (last xs)

runTestScript :: Script -> Behavior IO Text Text -> IO Script
runTestScript (Script script) bot =
  fmap (Script . snd . snd) $
    flip runStateT (inputs, []) $
      runExceptT $
        loop $
          boundedAnnihilation
            testServer
            (liftBehavior bot)
  where
    inputs = fmap input script
