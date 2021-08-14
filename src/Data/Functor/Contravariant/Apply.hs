{-# LANGUAGE FunctionalDependencies #-}
module Data.Functor.Contravariant.Apply
( -- * Contravariant applicative functors
  Contrapply(..)
) where

import Data.Functor.Continuation
import Data.Functor.Contravariant.CPS
import Data.Profunctor.Cofun
import Data.Profunctor.Fun

-- Contravariant applicative functors

class ContravariantCPS r k => Contrapply r k | k -> r where
  {-# MINIMAL coliftC2 | (<&>) #-}

  coliftC2 :: ((b >-r-~ c) ~~r~> a) -> k a -> k b -> k c
  coliftC2 f a b = f <#> a <&> b

  (<&>) :: k (a >-r-~ b) -> k a -> k b
  (<&>) = coliftC2 (fun (!))

  infixl 3 <&>
