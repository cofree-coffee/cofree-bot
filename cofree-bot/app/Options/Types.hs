{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Options.Types
  ( ClientSessionF (..),
    toClientSession,
    MatrixServer (..),
    MatrixToken (..),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Functor.Barbie
  ( AllBF,
    ApplicativeB,
    ConstraintsB,
    FunctorB,
    Rec (Rec),
    TraversableB,
    bpure,
    bzipWith,
  )
import Data.Text (Text)
import GHC.Generics (Generic, M1 (M1))
import Network.Matrix.Client
  ( ClientSession,
    MatrixToken (..),
    createSession,
  )

--------------------------------------------------------------------------------

newtype MonoidB b f = MonoidB (b f)

instance (ApplicativeB b, Alternative f) => Semigroup (MonoidB b f) where
  MonoidB t1 <> MonoidB t2 = MonoidB $ bzipWith (<|>) t1 t2

instance (ApplicativeB b, Alternative f) => Monoid (MonoidB b f) where
  mempty = MonoidB $ bpure empty

--------------------------------------------------------------------------------

data ClientSessionF f = ClientSessionF
  { matrixServer :: f MatrixServer,
    matrixToken :: f MatrixToken
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)
  deriving (Semigroup, Monoid) via (MonoidB ClientSessionF f)

deriving instance (AllBF Aeson.FromJSON f ClientSessionF) => Aeson.FromJSON (ClientSessionF f)

toClientSession :: ClientSessionF Maybe -> IO (Maybe ClientSession)
toClientSession ClientSessionF {..} = do
  sequence $ liftA2 createSession (coerce matrixServer) matrixToken

--------------------------------------------------------------------------------

newtype MatrixServer = MatrixServer {getMatrixServer :: Text}
  deriving newtype (Read, FromJSON, ToJSON)

instance FromJSON MatrixToken where
  parseJSON = Aeson.withText "MatrixToken" (pure . MatrixToken)

deriving newtype instance Read MatrixToken
