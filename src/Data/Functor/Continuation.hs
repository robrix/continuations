-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
) where

import Data.Functor.Contravariant

-- Continuations

newtype r ! a = K { (!) :: a -> r }

infixl 8 !

instance Contravariant ((!) r) where
  contramap f = K . (. f) . (!)
