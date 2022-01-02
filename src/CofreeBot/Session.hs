module CofreeBot.Session where

import Data.Text qualified as T

newtype MatrixServer = MatrixServer { getMatrixServer :: T.Text }
