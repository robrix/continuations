{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Negation
( -- * Continuation monad
  Neg(..)
  -- * Construction
, neg
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
  liftA2 f (Neg a) (Neg b) = neg (\ c -> a ! inK (\ a -> b ! inK (\ b -> c ! f a b)))


-- Construction

neg :: Continuation r k => (k a -> r) -> Neg k a
neg = Neg . inK


-- Elimination

runNeg :: Continuation r k => k a -> k (Neg k a)
runNeg = adjunct getNeg
