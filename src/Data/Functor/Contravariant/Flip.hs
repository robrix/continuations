-- | 'Contravariant' functors from 'Profunctor's.
--
-- Generalizes "Data.Functor.Contravariant".'Op' to any 'Profunctor'.
module Data.Functor.Contravariant.Flip
( -- * Flipped profunctors
  Flip(..)
) where

import Data.Functor.Contravariant
import Data.Profunctor

-- Flipped profunctors.

newtype Flip p a b = Flip { getFlip :: p b a }

instance Profunctor p => Contravariant (Flip p a) where
  contramap f = Flip . lmap f . getFlip
