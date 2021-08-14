{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
  -- * Continuation abstraction
, Continuation(..)
  -- ** Construction
, idK
, inK1
, inK2
  -- ** Elimination
, (!)
  -- ** Coercion
, coerceK
  -- ** Computation
, adjunct
  -- ** Defaults
, tabulateContinuation
, indexContinuation
  -- * Double negation
, type (!!)
  -- ** Construction
, in2K
) where

import           Control.Applicative (liftA2)
import qualified Control.Category as Cat
import           Data.Functor.Contravariant

-- Continuations

-- | Continuations, represented as functions. Note that the type parameters are in the opposite order, making this type a 'Contravariant' functor.
newtype r ! a = K { runK :: a -> r }

infixl 7 !

instance Cat.Category (!) where
  id = idK
  j . k = K (exK k . exK j)

instance Contravariant ((!) r) where
  contramap f = K . (. f) . (!)

instance Semigroup r => Semigroup (r ! a) where
  (<>) = inK2 (liftA2 (<>))

instance Monoid r => Monoid (r ! a) where
  mempty = K mempty

class Contravariant k => Continuation r k | k -> r where
  inK :: (a -> r) -> k a
  exK :: k a -> (a -> r)

instance Continuation r ((!) r) where
  inK = K
  exK = runK

instance Continuation Bool Predicate where
  inK = Predicate
  exK = getPredicate

instance Continuation r (Op r) where
  inK = Op
  exK = getOp


-- Construction

idK :: Continuation r k => k r
idK = inK id


inK1 :: Continuation r k => ((a -> r) -> (b -> r)) -> (k a -> k b)
inK1 f = inK . f . (!)

inK2 :: Continuation r k => ((a -> r) -> (b -> r) -> (c -> r)) -> (k a -> k b -> k c)
inK2 f a b = inK (exK a `f` exK b)


-- Elimination

(!) :: Continuation r k => k a -> (a -> r)
(!) = exK


-- Coercion

coerceK :: (Continuation r j, Continuation r k) => j a -> k a
coerceK = inK . (!)


-- Computation

adjunct :: Continuation r k => (a -> k b) -> (b -> k a)
adjunct f a = inK ((! a) . f)


-- Defaults

tabulateContinuation :: Continuation r k => (a -> r) -> k a
tabulateContinuation = inK

indexContinuation :: Continuation r k => k a -> (a -> r)
indexContinuation = (!)


-- Double negation

type k !! a = k (k a)

infixr 7 !!


-- Construction

in2K :: Continuation r k => a -> k !! a
in2K = inK . flip (!)
