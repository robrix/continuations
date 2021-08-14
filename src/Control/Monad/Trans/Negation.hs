module Control.Monad.Trans.Negation
( -- * Continuation monad
  type (!!)(..)
) where

import Data.Functor.Contravariant

newtype k !! a = Neg { runNeg :: k (k a) }

instance Contravariant k => Functor ((!!) k) where
  fmap f (Neg k) = Neg ((f >$<) >$< k)
  a <$ Neg k = Neg ((a >$) >$< k)
