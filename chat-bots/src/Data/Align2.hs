module Data.Align2
  ( Semialign2 (..),
  )
where

--------------------------------------------------------------------------------

import Data.Chat.Utils

--------------------------------------------------------------------------------

class Semialign2 p where
  align2 :: p a b -> p c d -> p (a /+\ c) (b /+\ d)
  alignWith2 :: (a /+\ c -> e) -> (b /+\ d -> f) -> p a b -> p c d -> p e f
