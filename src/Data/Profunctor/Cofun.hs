module Data.Profunctor.Cofun
( -- * Co-functions
  Cofun(..)
  -- * Construction
, (>-)
  -- * Elimination
, withCofun
  -- * Computation
, cocurry
, uncocurry
, coap
) where

import Data.Bifunctor.Disjunction
import Data.Functor.Continuation
import Data.Functor.Contravariant
import Data.Profunctor.Fun

-- Co-functions

data Cofun r b a = (:>-) { coreturn :: r ! b, coconst :: a }

infixr 0 :>-


-- Construction

(>-) :: (r ! b) -> a -> Cofun r b a
(>-) = (:>-)

infixr 0 >-


-- Elimination

withCofun :: Cofun r b a -> s ! ((r ! b) -> (s ! a))
withCofun (b :>- a) = K (\ f -> f b ! a)


-- Computation

cocurry :: Disj d => (c -> a `d` b) -> Cofun r b c ~~r~> a
cocurry f = Fun (\ k -> K (\ (b :>- c) -> (k <!!> b) ! f c))

uncocurry :: Disj d => (Cofun r b c ~~r~> a) -> (c ~~r~> (a `d` b))
uncocurry f = Fun (\ k -> K (\ c -> f # inlK k ! (inrK k >- c)))

coap :: Disj d => c ~~r~> (Cofun r b c `d` b)
coap = Fun (\ k -> (inrK k >-) >$< inlK k)
