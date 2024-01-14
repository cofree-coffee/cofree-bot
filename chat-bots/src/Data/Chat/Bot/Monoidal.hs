{-# LANGUAGE RankNTypes #-}

-- | Tensor Products on 'Bot'
module Data.Chat.Bot.Monoidal
  ( nudge,
    nudgeLeft,
    nudgeRight,
    (/\),
    (/.\),
    (/+\),
    (\/),
  )
where

--------------------------------------------------------------------------------

import Control.Monad.ListT (toListT)
import Data.Align
import Data.Bifunctor.Monoidal.Specialized (split')
import Data.Chat.Bot (Bot (..), invmapBot)
import Data.Chat.Utils (type (/+\), type (/\), type (\*/), type (\/))
import Data.Profunctor (Profunctor (..), Strong (..))
import Data.These (These (..))
import Data.Trifunctor.Monoidal qualified as Trifunctor

--------------------------------------------------------------------------------

-- | Given the sum of two bots, produce a bot who receives the sum of
-- the inputs to the input bots and produces a wedge product of their
-- outputs.
nudge ::
  Monad m => Bot m s i o \/ Bot m s i' o' -> Bot m s (i \/ i') (o \*/ o')
nudge =
  either
    ( \(Bot b) -> Bot $ \s ->
        either
          (fmap (fmap (first' (Just . Left))) $ b s)
          (const $ pure $ (,) Nothing s)
    )
    ( \(Bot b) -> Bot $ \s ->
        either
          (const $ pure $ (,) Nothing s)
          (fmap (fmap (first' (Just . Right))) $ b s)
    )

-- | Nudge a bot into the left side of a bot with a summed input and
-- wedge product output.
nudgeLeft :: Monad m => Bot m s i o -> Bot m s (i \/ i') (o \*/ o')
nudgeLeft = nudge . Left

-- | Nudge a bot into the right side of a bot with a summed input and
-- wedge product output.
nudgeRight :: Monad m => Bot m s i' o' -> Bot m s (i \/ i') (o \*/ o')
nudgeRight = nudge . Right

-- | Tuple the states and outputs of two bots who operate on the same
-- input @i@.
infixr 9 /\

(/\) :: Monad m => Bot m s i o -> Bot m s' i o' -> Bot m (s /\ s') i (o /\ o')
(/\) b1 = lmap split' . curry Trifunctor.combine b1

-- | Runs two bots and then interleaves their output.
infixr 9 /+\

(/+\) ::
  Monad m => Bot m s i o -> Bot m s' i' o' -> Bot m (s /\ s') (i /+\ i') (o /+\ o')
(/+\) b1 b2 = Trifunctor.combine (b1, b2)

-- | Runs two bots on the same input and then interleaves their
-- output, sequencing if they both return an output for the same
-- input.
infixr 9 /.\

(/.\) :: Monad m => Bot m s i o -> Bot m s' i o -> Bot m (s /\ s') i o
(/.\) (Bot b1) (Bot b2) = Bot $ \(s1, s2) i -> do
  align (b1 s1 i) (b2 s2 i) >>= \case
    This (o, s1') -> pure (o, (s1', s2))
    That (o', s2') -> pure (o', (s1, s2'))
    These (o, s1') (o', s2') -> toListT [(o, (s1', s2)), (o', (s1', s2'))]

-- | Sum the inputs and outputs of two bots who operate on the same
-- state @s@.
--
-- This allows us to combine the behaviors of two bots such that only
-- one or the other bot will be executed depending on the input
-- provided.
infixr 9 \/

(\/) :: Monad m => Bot m s i o -> Bot m s i' o' -> Bot m s (i \/ i') (o \/ o')
(\/) b1 b2 = invmapBot fst split' $ Trifunctor.combine (b1, b2)
