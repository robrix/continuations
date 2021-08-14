{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Negation
( -- * Continuation monad
  NegT(..)
  -- * Construction
, neg
, neg1
, neg2
  -- * Elimination
, runNeg
) where

import Control.Applicative (liftA2)
import Data.Functor.Continuation

newtype NegT k m a = NegT { getNegT :: k (k a) }

instance Contravariant k => Functor (NegT k m) where
  fmap f (NegT k) = NegT (contramap (contramap f) k)
  a <$ NegT k = NegT (contramap (a >$) k)

instance Continuation r k => Applicative (NegT k m) where
  pure = NegT . unit
  liftA2 f = neg2 (\ a b c -> a (b . (c .) . f))

instance Continuation r k => Monad (NegT k m) where
  m >>= f = NegT (adjunct @_ @k (adjunct getNegT . adjunct (getNegT . f)) m)


-- Construction

neg :: Continuation r k => ((a -> r) -> r) -> NegT k m a
neg = NegT . in2K

neg1 :: Continuation r k => (((a -> r) -> r) -> ((b -> r) -> r)) -> (NegT k m a -> NegT k m b)
neg1 f = NegT . in2K . f . ex2K . getNegT

neg2 :: Continuation r k => (((a -> r) -> r) -> ((b -> r) -> r) -> ((c -> r) -> r)) -> (NegT k m a -> NegT k m b -> NegT k m c)
neg2 f a b = NegT (in2K (ex2K (getNegT a) `f` ex2K (getNegT b)))


-- Elimination

runNeg :: Continuation r k => k a -> k (NegT k m a)
runNeg = adjunct getNegT
