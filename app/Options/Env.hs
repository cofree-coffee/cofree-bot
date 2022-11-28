-- | Subroutine for fetching Client Session data from the Environment.
module Options.Env
  ( fromEnv,
  )
where

--------------------------------------------------------------------------------

import Data.Functor.Barbie
import Data.Functor.Compose
import Options.Types
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Env

readEnv :: Read a => String -> (IO `Compose` Maybe) a
readEnv envKey =
  Compose $ lookupEnv envKey >>= pure . maybe Nothing readMaybe

fromEnv :: IO (ClientSessionF Maybe)
fromEnv =
  bsequence $
    ClientSessionF
      { matrixServer = readEnv "COFREE_BOT_MATRIX_SERVER",
        matrixToken = readEnv "COFREE_BOT_MATRIX_TOKEN"
      }
