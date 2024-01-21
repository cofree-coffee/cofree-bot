{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Main where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Control.Monad.ListT
import Data.Functor.Identity
import Hedgehog (Gen)
import Hedgehog.Classes qualified as Classes
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, hspec, it)
import Test.Hspec.Hedgehog ()

--------------------------------------------------------------------------------

satisfies :: Gen a -> (Gen a -> Classes.Laws) -> Spec
satisfies gen laws = do
  describe (className <> " instance") $ do
    forM_ properties $ \(name, p) -> do
      it ("satisfies " <> name) (propertyTest p)
  where
    Classes.Laws className properties = laws gen

satisfies1 ::
  forall c f.
  (c f, forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)) =>
  (forall a. Gen a -> Gen (f a)) ->
  ((c f, forall x. Eq x => Eq (f x), forall x. Show x => Show (f x)) => (forall x. Gen x -> Gen (f x)) -> Classes.Laws) ->
  Spec
satisfies1 gen laws = case laws gen of
  Classes.Laws className properties -> do
    describe (className <> " instance") $ do
      forM_ properties $ \(name, p) -> do
        it ("satisfies " <> name) (propertyTest p)

aGoodSize :: Range Int
aGoodSize = Range.linear 0 10

genString :: Gen String
genString = Gen.string aGoodSize Gen.ascii

genListT :: (Applicative m) => Gen a -> Gen (ListT m a)
genListT g = toListT <$> Gen.list aGoodSize g

main :: IO ()
main =
  hspec $ do
    spec

spec :: Spec
spec = do
  describe "ListT" $ do
    genListT @Identity genString `satisfies` Classes.showLaws
    genListT @Identity genString `satisfies` Classes.eqLaws
    satisfies1 @Functor @(ListT Identity) genListT Classes.functorLaws
    satisfies1 @Applicative @(ListT Identity) genListT Classes.applicativeLaws
    satisfies1 @Alternative @(ListT Identity) genListT Classes.alternativeLaws
    satisfies1 @Monad @(ListT Identity) genListT Classes.monadLaws
