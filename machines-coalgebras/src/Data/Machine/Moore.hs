{-# LANGUAGE CPP #-}

module Data.Machine.Moore where

--------------------------------------------------------------------------------

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Category.Cartesian (Semicartesian (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Monoidal ((|**|))
import Data.Bifunctor.Monoidal qualified as Bifunctor
import Data.Functor.Identity (Identity (..))
import Data.Profunctor (Closed (..), Costrong (..), Profunctor (..))
import Data.Profunctor.Rep (Corepresentable (..))
import Data.Profunctor.Sieve (Cosieve (..))
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------

-- | The fixed point of a 'MooreM' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
newtype MooreM' m i o = MooreM' {runMooreM' :: m (o, i -> MooreM' m i o)}
  deriving (Functor)

instance (Applicative m) => Applicative (MooreM' m i) where
  pure :: o -> MooreM' m i o
  pure o = let r = MooreM' $ pure (o, const r) in r

#if !MIN_VERSION_base(4,18,0)
  (<*>) :: MooreM' m i (a -> b) -> MooreM' m i a -> MooreM' m i b
  (<*>) m1 m2 = dimap split (uncurry ($)) $ m1 |**| m2
#else
  liftA2 :: (o -> o' -> o'') -> MooreM' m i o -> MooreM' m i o' -> MooreM' m i o''
  liftA2 f m1 m2 = dimap split (uncurry f) $ m1 |**| m2
#endif

instance (Applicative m) => Bifunctor.Semigroupal (->) (,) (,) (,) (MooreM' m) where
  combine :: (MooreM' m i o, MooreM' m i' o') -> MooreM' m (i, i') (o, o')
  combine (MooreM' m1, MooreM' m2) =
    MooreM' $ liftA2 (\(o, m1') (o', m2') -> ((o, o'), \(i, i') -> Bifunctor.combine (m1' i, m2' i'))) m1 m2

instance (Applicative m) => Bifunctor.Unital (->) () () () (MooreM' m) where
  introduce :: () -> MooreM' m () ()
  introduce () = MooreM' $ pure ((), Bifunctor.introduce)

instance (Applicative m) => Bifunctor.Unital (->) Void () () (MooreM' m) where
  introduce :: () -> MooreM' m Void ()
  introduce () = MooreM' $ pure ((), absurd)

instance (Applicative m) => Bifunctor.Monoidal (->) (,) () (,) () (,) () (MooreM' m)

instance (Applicative m, Semigroup o) => Semigroup (MooreM' m i o) where
  (<>) :: MooreM' m i o -> MooreM' m i o -> MooreM' m i o
  MooreM' m1 <> MooreM' m2 = MooreM' $ liftA2 (<>) m1 m2

instance (Applicative m, Monoid o) => Monoid (MooreM' m i o) where
  mempty :: MooreM' m i o
  mempty = MooreM' $ pure mempty

instance (Functor m) => Profunctor (MooreM' m) where
  dimap :: (i' -> i) -> (o -> o') -> MooreM' m i o -> MooreM' m i' o'
  dimap f g (MooreM' moore) =
    MooreM' $ fmap (bimap g (dimap f (dimap f g))) moore

instance (Functor m) => Costrong (MooreM' m) where
  unfirst :: MooreM' m (i, d) (o, d) -> MooreM' m i o
  unfirst (MooreM' moore) = MooreM' $ fmap (\((o, d), m) -> (o, \i -> unfirst (m (i, d)))) moore

--------------------------------------------------------------------------------

-- | Lift a monad morphism from @m@ to @n@ into a monad morphism from
-- @Env m s o i@ to @Env n s o i@
hoistMooreM' :: (Functor n) => (forall x. m x -> n x) -> MooreM' m o i -> MooreM' n o i
hoistMooreM' f (MooreM' server) = MooreM' $ fmap (fmap (fmap (hoistMooreM' f))) $ f server

-- | Lift a computation on the monad @m@ to the constructed monad @t
-- m@ in the context of a 'Server'.
liftMooreM' :: (Functor (t m), Monad m, MonadTrans t) => MooreM' m o i -> MooreM' (t m) o i
liftMooreM' = hoistMooreM' lift

--------------------------------------------------------------------------------

-- | The fixed point of a 'Moore' Machine. By taking the fixpoint we
-- are able to hide the state parameter @s@.
type Moore' = MooreM' Identity

instance Cosieve Moore' [] where
  cosieve :: Moore' i o -> [i] -> o
  cosieve (runIdentity . runMooreM' -> (o, _)) [] = o
  cosieve (runIdentity . runMooreM' -> (_, k)) (x : xs) = cosieve (k x) xs

instance Corepresentable Moore' where
  type Corep Moore' = []

  cotabulate :: (Corep Moore' i -> o) -> Moore' i o
  cotabulate f = MooreM' $ Identity (f [], \i -> cotabulate (f . (i :)))

instance Closed Moore' where
  closed :: Moore' i o -> Moore' (x -> i) (x -> o)
  closed m = cotabulate $ \fs x -> cosieve m (fmap ($ x) fs)

--------------------------------------------------------------------------------

-- | Feed inputs into a 'MooreM'' Machine and then observe the final
-- result.
processMooreM' :: (Monad m) => [i] -> MooreM' m i o -> m o
processMooreM' [] (MooreM' moore) = fmap fst moore
processMooreM' (i : xs) (MooreM' moore) = do
  (_, nextMoore) <- moore
  processMooreM' xs (nextMoore i)

-- | Feed inputs into a 'Moore'' Machine and then observe the final
-- result.
processMoore' :: [i] -> Moore' i o -> o
processMoore' = flip cosieve
