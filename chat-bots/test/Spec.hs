{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Main where

--------------------------------------------------------------------------------

import Test.Hspec (Spec, describe, hspec)
import Test.Hspec.Hedgehog ()

--------------------------------------------------------------------------------

main :: IO ()
main =
  hspec $ do
    spec

spec :: Spec
spec = do
  describe "TODO" $ do
    pure ()
