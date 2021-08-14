{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Negation
( -- * Continuation monad
  Neg(..)
  -- * Construction
, neg
, neg1
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
  liftA2 f a b = neg (\ c -> runNeg (inK (\ a -> runNeg (inK (\ b -> c ! f a b)) ! b)) ! a)


-- Construction

neg :: Continuation r k => (k a -> r) -> Neg k a
neg = Neg . inK

neg1 :: Continuation r k => ((k a -> r) -> (k b -> r)) -> (Neg k a -> Neg k b)
neg1 f = neg . f . exK . getNeg


-- Elimination

runNeg :: Continuation r k => k a -> k (Neg k a)
runNeg = adjunct getNeg
