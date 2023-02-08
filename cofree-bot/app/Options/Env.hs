-- | Subroutine for fetching Client Session data from the Environment.
module Options.Env
  ( fromEnv,
    readEnv,
  )
where

--------------------------------------------------------------------------------

import Data.Functor.Barbie
import Data.Functor.Compose
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Types
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Env

readEnv :: (Text -> a) -> String -> (IO `Compose` Maybe) a
readEnv cstr envKey = Compose $ fmap (cstr . Text.pack) <$> lookupEnv envKey

fromEnv :: IO (ClientSessionF Maybe)
fromEnv =
  bsequence $
    ClientSessionF
      { matrixServer = readEnv MatrixServer "COFREE_BOT_MATRIX_SERVER",
        matrixToken = readEnv MatrixToken "COFREE_BOT_MATRIX_TOKEN"
      }
