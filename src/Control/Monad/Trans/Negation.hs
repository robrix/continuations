{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Trans.Negation
( -- * Continuation monad
  type (!!)(..)
) where

import Control.Applicative (liftA2)
import Data.Functor.Continuation

newtype k !! a = Neg { runNeg :: k (k a) }

instance Contravariant k => Functor ((!!) k) where
  fmap f (Neg k) = Neg (contramap (contramap f) k)
  a <$ Neg k = Neg (contramap (a >$) k)

instance Continuation r k => Applicative ((!!) k) where
  pure = Neg . in2K
  liftA2 f (Neg a) (Neg b) = Neg (inK (\ c -> a ! inK (\ a -> b ! inK (\ b -> c ! f a b))))


in2K :: Continuation r k => a -> k (k a)
in2K = inK . flip (!)
