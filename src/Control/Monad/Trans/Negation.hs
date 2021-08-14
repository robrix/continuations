module Control.Monad.Trans.Negation
( -- * Continuation monad
  type (!!)(..)
) where

newtype k !! a = Neg { runNeg :: k (k a) }
