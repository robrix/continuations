-- | 'Contravariant' functors from 'Profunctor's.
module Data.Functor.Contravariant.Flip
( -- * Flipped profunctors
  Flip(..)
) where

-- Flipped profunctors.

newtype Flip p a b = Flip { getFlip :: p b a }
