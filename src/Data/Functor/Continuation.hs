{-# LANGUAGE TypeFamilies #-}
-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
, Representable(..)
  -- * Elimination
, exlK
, exrK
, deMorganPair
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


-- Elimination

exlK :: Contravariant k => k a -> k (a, b)
exlK = contramap fst

exrK :: Contravariant k => k b -> k (a, b)
exrK = contramap snd


deMorganPair :: Contravariant k => Either (k a) (k b) -> k (a, b)
deMorganPair = contramap fst `either` contramap snd
