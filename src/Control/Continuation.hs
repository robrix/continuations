module Control.Continuation
( -- * Continuations
  type (!)(..)
) where

-- Continuations

newtype r ! a = K { (!) :: a -> r }
