{-# LANGUAGE FunctionalDependencies #-}
module Data.Functor.Contravariant.CPS
( -- * Contravariant continuation-passing style
  ContravariantCPS(..)
, Contravariant(..)
  -- ** Computation
, comap
) where

import Data.Functor.Continuation
import Data.Functor.Contravariant
import Data.Profunctor.Fun

-- Contravariant continuation-passing style

class Contravariant k => ContravariantCPS r k | k -> r where
  (<#>) :: (a' ~~r~> a) -> (k a -> k a')

  infixl 4 <#>

instance ContravariantCPS r ((!) r) where
  (<#>) = flip (#)

instance ContravariantCPS Bool Predicate where
  f <#> Predicate p = Predicate (K p # f !)

instance ContravariantCPS Ordering Comparison where
  f <#> Comparison c = Comparison (\ a b -> K (\ a -> K (c a) # f ! b) # f ! a)

instance ContravariantCPS Bool Equivalence where
  f <#> Equivalence e = Equivalence (\ a b -> K (\ a -> K (e a) # f ! b) # f ! a)

instance ContravariantCPS r (Op r) where
  f <#> Op k = Op (K k # f !)


-- Computation

comap :: Contravariant f => (a' -> a) -> (f a -> f a')
comap = contramap
