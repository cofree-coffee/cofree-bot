module Data.Machine
  ( module M,
    Fix (..),
    annihilate,
    loop,
  )
where

--------------------------------------------------------------------------------

import Data.Fix (Fix (..))
import Data.Machine.Mealy as M
import Data.Machine.Moore as M

--------------------------------------------------------------------------------

-- | "An Ay^B Mealy Machine is the 'universal thing' that interacts
-- with a By^A Moore Machine. Its the universal thing that can be put
-- together with a By^A Moore Machine. They're not just two different
-- definitions, they are dual in certain sense." -- David Spivak
annihilate :: (Monad m) => MooreM' m o i -> MealyM' m i o -> Fix m
annihilate (MooreM' moore) (MealyM' mealy) = Fix $ do
  (i, nextMoore) <- moore
  (o, mealy') <- mealy i
  let moore' = nextMoore o
  pure $ annihilate moore' mealy'

-- | Recursively unfold fixed point @Fix m@.
loop :: (Monad f) => Fix f -> f x
loop (Fix x) = x >>= loop
