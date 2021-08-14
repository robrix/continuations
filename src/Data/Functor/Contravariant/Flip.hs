{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | 'Contravariant' functors from 'Profunctor's.
--
-- Generalizes "Data.Functor.Contravariant".'Op' to any 'Profunctor'.
module Data.Functor.Contravariant.Flip
( -- * Flipped profunctors
  Flip(..)
  -- * CPS-traversable profunctors
, ProfunctorCPS(..)
) where

import Control.Arrow
import Data.Functor.Continuation
import Data.Functor.Contravariant.CPS
import Data.Profunctor
import Data.Profunctor.Fun
import Data.Profunctor.Rep
import Data.Profunctor.Sieve

-- Flipped profunctors.

newtype Flip p a b = Flip { getFlip :: p b a }

instance Profunctor p => Contravariant (Flip p a) where
  contramap f = Flip . lmap f . getFlip

instance (Representable p, r ~ Rep p a) => Continuation r (Flip p a) where
  inK f = Flip (tabulate f)
  exK (Flip r) = sieve r

instance ProfunctorCPS r p => ContravariantCPS r (Flip p a) where
  f <#> a = Flip (lmapCPS f (getFlip a))


-- CPS-traversable profunctors

class Profunctor p => ProfunctorCPS r p | p -> r where
  {-# MINIMAL dimapCPS | (lmapCPS, rmapCPS) #-}

  dimapCPS :: (a' ~~r~> a) -> (b ~~r~> b') -> p a b -> p a' b'
  dimapCPS f g = lmapCPS f . rmapCPS g

  lmapCPS :: (a' ~~r~> a) -> p a b -> p a' b
  lmapCPS = (`dimapCPS` arr id)

  rmapCPS :: (b ~~r~> b') -> p a b -> p a b'
  rmapCPS = (arr id `dimapCPS`)
