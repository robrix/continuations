module Data.Profunctor.Cofun
( -- * Co-functions
  Cofun(..)
  -- * Mixfix syntax
, type (>-)
, type (-~)
  -- * Construction
, (>-)
  -- * Elimination
, withCofun
, elimCofun
  -- * Computation
, cocurry
, uncocurry
, coap
) where

import Data.Bifunctor.Disjunction
import Data.Functor.Continuation
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Profunctor.Fun.Internal

-- Co-functions

data Cofun r b a = (:>-) { coreturn :: r ! b, coconst :: a }

infixr 0 :>-

instance Profunctor (Cofun r) where
  dimap f g (b :>- a) = contramap f b >- g a
  lmap f (b :>- a) = contramap f b >- a
  rmap g (b :>- a) = b >- g a


-- Mixfix syntax

type a >-r = Cofun r a
type r-~ b = r b

infixr 1 >-
infixr 0 -~


-- Construction

(>-) :: (r ! b) -> a -> (b >-r-~ a)
(>-) = (:>-)


-- Elimination

withCofun :: (b >-r-~ a) -> s ! ((r ! b) -> (s ! a))
withCofun (b :>- a) = K (\ f -> f b ! a)

elimCofun :: (b ~~r~> a) -> r ! (a >-r-~ b)
elimCofun f = K (\ (a :>- b) -> f # a ! b)


-- Computation

cocurry :: Disj d => (c -> a `d` b) -> (b >-r-~ c) ~~r~> a
cocurry f = fun (\ k (b :>- c) -> (k <!!> b) ! f c)

uncocurry :: Disj d => ((b >-r-~ c) ~~r~> a) -> (c ~~r~> (a `d` b))
uncocurry f = fun (\ k c -> f # inlK k ! (inrK k >- c))

coap :: Disj d => c ~~r~> ((b >-r-~ c) `d` b)
coap = Fun (\ k -> (inrK k >-) >$< inlK k)
