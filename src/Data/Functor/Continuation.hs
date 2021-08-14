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
, constK
, inK1
, inK1'
, inK2
, in2K
  -- ** Elimination
, (!)
, exK1
, exK1'
, exK2
  -- ** Coercion
, coerceK
  -- ** Computation
, adjunct
  -- ** Defaults
, tabulateContinuation
, indexContinuation
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
  contramap f = inK1 (. f)

instance Semigroup r => Semigroup (r ! a) where
  (<>) = inK2 (<>)

instance Monoid r => Monoid (r ! a) where
  mempty = K mempty

instance Num r => Num (r ! a) where
  (+) = inK2 (liftA2 (+))
  (*) = inK2 (liftA2 (*))
  (-) = inK2 (liftA2 (-))
  abs = inK1 (fmap abs)
  signum = inK1 (fmap signum)
  fromInteger = constK . fromInteger

instance Fractional r => Fractional (r ! a) where
  (/) = inK2 (liftA2 (/))
  recip = inK1 (fmap recip)
  fromRational = constK . fromRational

instance Floating r => Floating (r ! a) where
  pi = constK pi
  exp = inK1 (fmap exp)
  sqrt = inK1 (fmap sqrt)
  log = inK1 (fmap log)
  sin = inK1 (fmap sin)
  tan = inK1 (fmap tan)
  cos = inK1 (fmap cos)
  asin = inK1 (fmap asin)
  atan = inK1 (fmap atan)
  acos = inK1 (fmap acos)
  sinh = inK1 (fmap sinh)
  tanh = inK1 (fmap tanh)
  cosh = inK1 (fmap cosh)
  asinh = inK1 (fmap asinh)
  atanh = inK1 (fmap atanh)
  acosh = inK1 (fmap acosh)
  (**) = inK2 (liftA2 (**))
  logBase = inK2 (liftA2 logBase)


-- Continuation abstraction

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

constK :: Continuation r k => r -> k b
constK = inK . const


inK1 :: Continuation r k => ((a -> r) -> (b -> r)) -> (k a -> k b)
inK1 f = inK1' (f . (!))

inK1' :: Continuation r k => (k a -> (b -> r)) -> (k a -> k b)
inK1' f = inK . f

inK2 :: Continuation r k => ((a -> r) -> (b -> r) -> (c -> r)) -> (k a -> k b -> k c)
inK2 f a b = inK (exK a `f` exK b)


in2K :: Continuation r k => a -> k (k a)
in2K = inK . flip (!)


-- Elimination

(!) :: Continuation r k => k a -> (a -> r)
(!) = exK


exK1 :: Continuation r k => (k a -> k b) -> ((a -> r) -> (b -> r))
exK1 f = exK . f . inK

exK1' :: Continuation r k => (k a -> k b) -> (k a -> (b -> r))
exK1' f = exK . f

exK2 :: Continuation r k => (k a -> k b -> k c) -> ((a -> r) -> (b -> r) -> (c -> r))
exK2 f a b = exK (inK a `f` inK b)


-- Coercion

coerceK :: (Continuation r j, Continuation r k) => j a -> k a
coerceK = inK . (!)


-- Computation

adjunct :: (Continuation r j, Continuation r k) => (a -> j b) -> (b -> k a)
adjunct f a = inK ((! a) . f)


-- Defaults

tabulateContinuation :: Continuation r k => (a -> r) -> k a
tabulateContinuation = inK

indexContinuation :: Continuation r k => k a -> (a -> r)
indexContinuation = (!)
