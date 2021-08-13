-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
  -- * Elimination
, inlK
, inrK
) where

import Data.Functor.Contravariant

-- Continuations

newtype r ! a = K { (!) :: a -> r }

infixl 7 !

instance Contravariant ((!) r) where
  contramap f = K . (. f) . (!)


-- Elimination

inlK :: Contravariant k => k (Either a b) -> k a
inlK = contramap Left

inrK :: Contravariant k => k (Either a b) -> k b
inrK = contramap Right
