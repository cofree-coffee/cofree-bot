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

import Control.Monad.ListT (alignListT, toListT)
import Data.Bifunctor (Bifunctor (..))
import Data.Chat.Bot (Bot (..))
import Data.Chat.Utils (type (/+\), type (/\), type (\*/), type (\/))
import Data.Functor ((<&>))
import Data.Profunctor (Strong (..))
import Data.These (These (..))

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
(/\) (Bot b1) (Bot b2) = Bot $ \(s, s') i -> do
  (nextState, responses) <- b1 s i
  (nextState', responses') <- b2 s' i
  pure $ (,) (nextState, nextState') (responses, responses')

-- | Runs two bots and then interleaves their output.
infixr 9 /+\

(/+\) ::
  Monad m => Bot m s i o -> Bot m s' i' o' -> Bot m (s /\ s') (i /+\ i') (o /+\ o')
(/+\) (Bot b1) (Bot b2) = Bot $ \(s, s') -> \case
  This i -> fmap (bimap This (,s')) $ b1 s i
  That i' -> fmap (bimap That (s,)) $ b2 s' i'
  These i i' ->
    alignListT (b1 s i) (b2 s' i') <&> \case
      This (o, _s) -> (This o, (s, s'))
      That (o', _s') -> (That o', (s, s'))
      These (o, s) (o', s') -> (These o o', (s, s'))

-- | Runs two bots on the same input and then interleaves their
-- output, sequencing if they both return an output for the same
-- input.
infixr 9 /.\

(/.\) :: Monad m => Bot m s i o -> Bot m s' i o -> Bot m (s /\ s') i o
(/.\) (Bot b1) (Bot b2) = Bot $ \(s1, s2) i -> do
  alignListT (b1 s1 i) (b2 s2 i) >>= \case
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
(\/) (Bot b1) (Bot b2) =
  Bot $ \s -> either (fmap (first Left) . b1 s) (fmap (first Right) . b2 s)
