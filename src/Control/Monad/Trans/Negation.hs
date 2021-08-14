{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Negation
( -- * Continuation monad
  Neg(..)
  -- * Construction
, neg
, in2K
) where

import Control.Applicative (liftA2)
import Data.Functor.Continuation

newtype Neg k a = Neg { runNeg :: k (k a) }

instance Contravariant k => Functor (Neg k) where
  fmap f (Neg k) = Neg (contramap (contramap f) k)
  a <$ Neg k = Neg (contramap (a >$) k)

instance Continuation r k => Applicative (Neg k) where
  pure = Neg . in2K
  liftA2 f (Neg a) (Neg b) = neg (\ c -> a ! inK (\ a -> b ! inK (\ b -> c ! f a b)))


-- Construction

neg :: Continuation r k => (k a -> r) -> Neg k a
neg = Neg . inK

in2K :: Continuation r k => a -> k (k a)
in2K = inK . flip (!)
