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
import Data.Machine.Mealy.Coalgebra (MealyM (..))
import Data.Profunctor (Profunctor (..), Strong (..))
import Data.These (These)
import Data.Trifunctor.Monoidal ((|*&&|), (|***|), (|*++|))

--------------------------------------------------------------------------------

-- | Given the sum of two bots, produce a bot who receives the sum of
-- the inputs to the input bots and produces a wedge product of their
-- outputs.
nudge ::
  (Monad m) => Either (MealyM m s i o) (MealyM m s i' o') -> MealyM m s (Either i i') (Maybe (Either o o'))
nudge =
  either
    ( \(MealyM b) -> MealyM $ \s ->
        either
          (fmap (fmap (first' (Just . Left))) $ b s)
          (const $ pure $ (,) Nothing s)
    )
    ( \(MealyM b) -> MealyM $ \s ->
        either
          (const $ pure $ (,) Nothing s)
          (fmap (fmap (first' (Just . Right))) $ b s)
    )

-- | Nudge a bot into the left side of a bot with a summed input and
-- wedge product output.
nudgeLeft :: (Monad m) => MealyM m s i o -> MealyM m s (Either i i') (Maybe (Either o o'))
nudgeLeft = nudge . Left

-- | Nudge a bot into the right side of a bot with a summed input and
-- wedge product output.
nudgeRight :: (Monad m) => MealyM m s i' o' -> MealyM m s (Either i i') (Maybe (Either o o'))
nudgeRight = nudge . Right

infixr 9 /\

(/\) :: (Applicative m) => MealyM m s i o -> MealyM m t i o' -> MealyM m (s, t) i (o, o')
(/\) m1 m2 = lmap split $ m1 |***| m2

infixr 9 /+\

(/+\) :: (Applicative m) => MealyM m s i o -> MealyM m t i' o' -> MealyM m (s, t) (These i i') (These o o')
(/+\) m1 m2 = m1 |*&&| m2

infixr 9 \/

(\/) :: (Applicative m) => MealyM m s i o -> MealyM m t i' o' -> MealyM m (s, t) (Either i i') (Either o o')
(\/) m1 m2 = m1 |*++| m2
