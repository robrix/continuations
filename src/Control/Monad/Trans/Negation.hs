{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Negation
( -- * Continuation monad
  Neg(..)
  -- * Construction
, neg
, neg1
, neg2
  -- * Elimination
, runNeg
) where

import Control.Applicative (liftA2)
import Data.Functor.Continuation

newtype Neg k a = Neg { getNeg :: k (k a) }

instance Contravariant k => Functor (Neg k) where
  fmap f (Neg k) = Neg (contramap (contramap f) k)
  a <$ Neg k = Neg (contramap (a >$) k)

instance Continuation r k => Applicative (Neg k) where
  pure = Neg . in2K0
  liftA2 f = neg2 (\ a b c -> a (b . (c .) . f))


-- Construction

neg :: Continuation r k => ((a -> r) -> r) -> Neg k a
neg = Neg . in2K

neg1 :: Continuation r k => (((a -> r) -> r) -> ((b -> r) -> r)) -> (Neg k a -> Neg k b)
neg1 f = Neg . in2K . f . ex2K . getNeg

neg2 :: Continuation r k => (((a -> r) -> r) -> ((b -> r) -> r) -> ((c -> r) -> r)) -> (Neg k a -> Neg k b -> Neg k c)
neg2 f a b = Neg (in2K (ex2K (getNeg a) `f` ex2K (getNeg b)))


-- Elimination

runNeg :: Continuation r k => k a -> k (Neg k a)
runNeg = adjunct getNeg
