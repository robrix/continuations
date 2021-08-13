{-# LANGUAGE TypeFamilies #-}
-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
  -- * Elimination
, inlK
, inrK
, (<!!>)
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

inlK :: Contravariant k => k (Either a b) -> k a
inlK = contramap Left

inrK :: Contravariant k => k (Either a b) -> k b
inrK = contramap Right

(<!!>) :: Representable k => k a -> k b -> k (Either a b)
a <!!> b = tabulate (either (index a) (index b))

infixr 3 <!!>
