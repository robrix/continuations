{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Continuations, modelled as functions wrapped in a contravariant functor.
module Data.Functor.Continuation
( -- * Continuations
  type (!)(..)
  -- * Contravariant
, Contravariant(..)
, Representable(..)
  -- * Construction
, idK
) where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Adjunction
import Data.Functor.Contravariant.Rep

-- Continuations

-- | Continuations, represented as functions. Note that the type parameters are in the opposite order, making this type a 'Contravariant' functor.
newtype r ! a = K { (!) :: a -> r }

infixl 7 !

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


-- Construction

idK :: a ! a
idK = K id
