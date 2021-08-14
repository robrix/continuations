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

cocurry :: (c -> Either a b) -> Fun r (Cofun r b c) a
cocurry f = Fun (\ k -> K (\ (b :>- c) -> (k <!!> b) ! f c))

uncocurry :: Fun r (Cofun r b c) a -> Fun r c (Either a b)
uncocurry f = Fun (\ k -> K (\ c -> f # inlK k ! (inrK k >- c)))

coap :: Disj d => Fun r c (Cofun r b c `d` b)
coap = Fun (\ k -> (inrK k >-) >$< inlK k)
