{-# LANGUAGE TypeFamilies #-}
-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
, Representable(..)
) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Rep

-- Continuations

newtype r ! a = K { (!) :: a -> r }

infixl 7 !

instance Contravariant ((!) r) where
  contramap f = K . (. f) . (!)

instance Representable ((!) r) where
  type Rep ((!) r) = r
  tabulate = K
  index = (!)
