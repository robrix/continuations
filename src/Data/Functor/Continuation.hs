module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
) where

import Data.Functor.Contravariant

-- Continuations

newtype r ! a = K { (!) :: a -> r }

infixl 8 !

instance Contravariant ((!) r) where
  contramap f (K k) = K (k . f)
