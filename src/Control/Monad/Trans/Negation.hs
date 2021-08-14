module Control.Monad.Trans.Negation
( -- * Continuation monad
  type (!!)(..)
) where

newtype k !! a = DN { runDN :: k (k a) }
