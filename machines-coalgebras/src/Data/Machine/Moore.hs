{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The fixed point of a 'MooreT Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
module Data.Machine.Moore
  ( MooreT (..),
    Moore,
    hoistMooreT,
    liftMooreT,
    processMooreT,
    processMoore,
  )
where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Functor.Identity (Identity (..))
import Data.Machine.MooreT (MooreT (..))
import Data.Profunctor.Sieve (Cosieve (..))
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

instance (Applicative m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MooreT m) where
  combine :: (MooreT m i o, MooreT m i' o') -> MooreT m (i, i') (o, o')
  combine (MooreT m1, MooreT m2) =
    MooreT $ liftA2 (\(o, m1') (o', m2') -> ((o, o'), \(i, i') -> Bifunctor.combine (m1' i, m2' i'))) m1 m2

instance (Applicative m) => Bifunctor.Unital (->) () () () (MooreT m) where
  introduce :: () -> MooreT m () ()
  introduce () = MooreT $ pure ((), Bifunctor.introduce)

instance (Applicative m) => Bifunctor.Unital (->) Void () () (MooreT m) where
  introduce :: () -> MooreT m Void ()
  introduce () = MooreT $ pure ((), absurd)

instance (Applicative m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MooreT m)

--------------------------------------------------------------------------------

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Env m s o i@ to @Env n s o i@
hoistMooreT :: (Functor n) => (forall x. m x -> n x) -> MooreT m o i -> MooreT n o i
hoistMooreT f (MooreT server) = MooreT $ fmap (fmap (fmap (hoistMooreT f))) $ f server

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'Server'.
liftMooreT :: (Functor (t m), Monad m, MonadTrans t) => MooreT m o i -> MooreT (t m) o i
liftMooreT = hoistMooreT lift

--------------------------------------------------------------------------------

-- | The fixed point of a 'Moore' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Moore = MooreT Identity

instance Cosieve Moore [] where
  cosieve :: Moore i o -> [i] -> o
  cosieve (runIdentity . runMooreT -> (o, _)) [] = o
  cosieve (runIdentity . runMooreT -> (_, k)) (x : xs) = cosieve (k x) xs

--------------------------------------------------------------------------------

-- | Feed inputs into a 'MooreT' Machine and then observe the final
-- result.
processMooreT :: (Monad m) => [i] -> MooreT m i o -> m o
processMooreT [] (MooreT moore) = fmap fst moore
processMooreT (i : xs) (MooreT moore) = do
  (_, nextMoore) <- moore
  processMooreT xs (nextMoore i)

-- | Feed inputs into a 'Moore'' Machine and then observe the final
-- result.
processMoore :: [i] -> Moore i o -> o
processMoore = flip cosieve
