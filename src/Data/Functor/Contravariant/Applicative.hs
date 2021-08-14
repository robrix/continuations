{-# LANGUAGE FunctionalDependencies #-}
module Data.Functor.Contravariant.Applicative
( -- * Contravariant applicative functors
  Contrapply(..)
, Contrapplicative(..)
) where

import Data.Functor.Continuation
import Data.Functor.Contravariant
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

instance Contrapply r ((!) r) where
  f <&> a = K (\ b -> f ! (a >- b))

instance Contrapply Bool Predicate where
  Predicate f <&> Predicate a = Predicate (\ b -> f (K a >- b))

instance Contrapply r (Op r) where
  Op f <&> Op a = Op (\ b -> f (K a >- b))


class Contrapply r k => Contrapplicative r k | k -> r where
  copure :: (b -> a) -> k (a >-r-~ b)

instance Contrapplicative r ((!) r) where
  copure = runCofun
