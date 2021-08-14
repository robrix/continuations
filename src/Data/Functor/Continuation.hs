{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
, Representable(..)
  -- * Continuation abstraction
, Continuation(..)
  -- ** Construction
, idK
, coerceK
, inK1
  -- ** Defaults
, tabulateContinuation
, indexContinuation
) where

import qualified Control.Category as Cat
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Adjunction
import           Data.Functor.Contravariant.Rep

-- Continuations

-- | Continuations, represented as functions. Note that the type parameters are in the opposite order, making this type a 'Contravariant' functor.
newtype r ! a = K { runK :: a -> r }

infixl 7 !

instance Cat.Category (!) where
  id = idK
  j . k = K ((k !) . (j !))

instance Contravariant ((!) r) where
  contramap f = K . (. f) . (!)

instance Representable ((!) r) where
  type Rep ((!) r) = r
  tabulate = K
  index = (!)

instance r ~ s => Adjunction ((!) r) ((!) s) where
  unit   a = K (! a)
  counit a = K (! a)

  leftAdjunct  f a = K ((! a) . f)
  rightAdjunct f a = K ((! a) . f)


class Contravariant k => Continuation r k | k -> r where
  inK :: (a -> r) -> k a
  (!) :: k a -> (a -> r)

instance Continuation r ((!) r) where
  inK = K
  (!) = runK

instance Continuation Bool Predicate where
  inK = Predicate
  (!) = getPredicate


-- Construction

idK :: Continuation r k => k r
idK = inK id

coerceK :: (Continuation r j, Continuation r k) => j a -> k a
coerceK = inK . (!)


inK1 :: Continuation r k => ((a -> r) -> (b -> r)) -> (k a -> k b)
inK1 f = inK . f . (!)


-- Defaults

tabulateContinuation :: Continuation r k => (a -> r) -> k a
tabulateContinuation = inK

indexContinuation :: Continuation r k => k a -> (a -> r)
indexContinuation = (!)
