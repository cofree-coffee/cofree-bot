module Data.Machine.Moore.Coalgebra.Monoidal
  ( (/\),
    (\/),
    (/+\),
  )
where

--------------------------------------------------------------------------------

import Control.Category.Cartesian (Semicartesian (..))
import Data.Machine.Moore.Coalgebra (MooreTC)
import Data.Profunctor (Profunctor (..))
import Data.These (These)
import Data.Trifunctor.Monoidal ((|*&*|), (|***|), (|*+*|))

--------------------------------------------------------------------------------
-- Tensors

infixr 9 /\

(/\) :: (Applicative m) => MooreTC m s i o -> MooreTC m t i o' -> MooreTC m (s, t) i (o, o')
(/\) m1 m2 = lmap split $ m1 |***| m2

infixr 9 /+\

(/+\) :: (Applicative m) => MooreTC m s i o -> MooreTC m t i' o' -> MooreTC m (s, t) (These i i') (o, o')
(/+\) m1 m2 = m1 |*&*| m2

infixr 9 \/

(\/) :: (Applicative m) => MooreTC m s i o -> MooreTC m t i' o' -> MooreTC m (s, t) (Either i i') (o, o')
(\/) m1 m2 = m1 |*+*| m2
