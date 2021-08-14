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
  pure = Neg . in2K
  liftA2 f = neg2 (\ a b c -> a (inK (\ a -> b (inK (\ b -> c ! f a b)))))


-- Construction

neg :: Continuation r k => (k a -> r) -> Neg k a
neg = Neg . inK

neg1 :: Continuation r k => ((k a -> r) -> (k b -> r)) -> (Neg k a -> Neg k b)
neg1 f = neg . f . exK . getNeg

neg2 :: Continuation r k => ((k a -> r) -> (k b -> r) -> (k c -> r)) -> (Neg k a -> Neg k b -> Neg k c)
neg2 f a b = neg (exK (getNeg a) `f` exK (getNeg b))


-- Elimination

runNeg :: Continuation r k => k a -> k (Neg k a)
runNeg = adjunct getNeg
