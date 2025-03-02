module Data.Machine.Mealy.Coalgebra.Monoidal
  ( nudge,
    nudgeLeft,
    nudgeRight,
    (/\),
    (/+\),
    (\/),
  )
where

--------------------------------------------------------------------------------

import Control.Category.Cartesian (split)
import Data.Machine.Mealy.Coalgebra (MealyTC (..))
import Data.Profunctor (Profunctor (..), Strong (..))
import Data.These (These)
import Data.Trifunctor.Monoidal ((|*&&|), (|***|), (|*++|))

--------------------------------------------------------------------------------

-- | Given the sum of two bots, produce a bot who receives the sum of
-- the inputs to the input bots and produces a wedge product of their
-- outputs.
nudge ::
  (Monad m) => Either (MealyTC m s i o) (MealyTC m s i' o') -> MealyTC m s (Either i i') (Maybe (Either o o'))
nudge =
  either
    ( \(MealyTC b) -> MealyTC $ \s ->
        either
          (fmap (fmap (first' (Just . Left))) $ b s)
          (const $ pure $ (,) Nothing s)
    )
    ( \(MealyTC b) -> MealyTC $ \s ->
        either
          (const $ pure $ (,) Nothing s)
          (fmap (fmap (first' (Just . Right))) $ b s)
    )

-- | Nudge a bot into the left side of a bot with a summed input and
-- wedge product output.
nudgeLeft :: (Monad m) => MealyTC m s i o -> MealyTC m s (Either i i') (Maybe (Either o o'))
nudgeLeft = nudge . Left

-- | Nudge a bot into the right side of a bot with a summed input and
-- wedge product output.
nudgeRight :: (Monad m) => MealyTC m s i' o' -> MealyTC m s (Either i i') (Maybe (Either o o'))
nudgeRight = nudge . Right

infixr 9 /\

(/\) :: (Applicative m) => MealyTC m s i o -> MealyTC m t i o' -> MealyTC m (s, t) i (o, o')
(/\) m1 m2 = lmap split $ m1 |***| m2

infixr 9 /+\

(/+\) :: (Applicative m) => MealyTC m s i o -> MealyTC m t i' o' -> MealyTC m (s, t) (These i i') (These o o')
(/+\) m1 m2 = m1 |*&&| m2

infixr 9 \/

(\/) :: (Applicative m) => MealyTC m s i o -> MealyTC m t i' o' -> MealyTC m (s, t) (Either i i') (Either o o')
(\/) m1 m2 = m1 |*++| m2
