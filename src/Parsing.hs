module Parsing where
import           Data.Void                      ( Void )

class Invariant f where
  invmap :: (b -> a) -> (a -> b) -> f a -> f b

class Invariant f => Invariant' f where
  unit1 :: f ()
  unit0 :: f Void
  (<×>) :: f a -> f b -> f (a, b)
  (<+>) :: f a -> f b -> f (Either a b)
  in1 :: f a -> (f a, f b)
  in2 :: f b -> (f a, f b)

  lstrong1
    :: (a, f b) -> f (a, b)

  lstrong2
    :: (a, f (a, b)) -> f b
