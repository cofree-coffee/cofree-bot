module Main where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Compose (Compose (..))
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Options.Env (readEnv)
import Options.Types (MatrixServer (MatrixServer))
import System.Environment (setEnv, unsetEnv)
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Hedgehog (forAll, hedgehog, (===))

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  readEnvSpec

-- Note: This spec is kind of silly, but the initial env var fetching
-- used 'read' and failed on the 'Text' instance requiring quoting.
readEnvSpec :: Spec
readEnvSpec = describe "readEnv" $ do
  it "can parse unquoted Text from the env" $ hedgehog $ do
    var <- fmap Text.unpack $ forAll $ Gen.text (Range.linear 1 50) Gen.alpha
    val <- forAll $ Gen.text (Range.linear 1 50) Gen.alpha

    liftIO $ setEnv var (Text.unpack val)
    val' <- liftIO $ getCompose $ readEnv id var
    liftIO $ unsetEnv var

    Just val === val'

  it "can parse a 'MatrixServer' from the env" $ hedgehog $ do
    var <- fmap Text.unpack $ forAll $ Gen.text (Range.linear 1 50) Gen.alpha
    val <- forAll $ Gen.text (Range.linear 1 50) Gen.alpha

    liftIO $ setEnv var (Text.unpack val)
    val' <- liftIO $ getCompose $ readEnv MatrixServer var
    liftIO $ unsetEnv var

    Just (MatrixServer val) === val'
