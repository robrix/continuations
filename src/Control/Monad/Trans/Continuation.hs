module Control.Monad.Trans.Continuation
( -- * Continuation monad
  type (!!)(..)
) where

newtype k !! a = DN { runDN :: k (k a) }
