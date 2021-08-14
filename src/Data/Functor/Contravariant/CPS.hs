{-# LANGUAGE FunctionalDependencies #-}
module Data.Functor.Contravariant.CPS
( -- * Contravariant continuation-passing style
  ContravariantCPS(..)
, Contravariant(..)
) where

import Data.Functor.Contravariant
import Data.Profunctor.Fun

-- Contravariant continuation-passing style

class Contravariant k => ContravariantCPS r k | k -> r where
  (<#>) :: (a' ~~r~> a) -> (k a -> k a')

  infixl 4 <#>
