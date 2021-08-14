module Data.Bifunctor.Disjunction
( -- * Disjunctions
  Disj(..)
, deMorganDisj
) where

import Data.Functor.Continuation

-- Disjunctions

class Disj d where
  inlK :: Contravariant k => k (a `d` b) -> k a
  inrK :: Contravariant k => k (a `d` b) -> k b
  (<!!>) :: Representable k => k a -> k b -> k (a `d` b)
  infixr 3 <!!>

instance Disj Either where
  inlK = contramap Left
  inrK = contramap Right
  a <!!> b = tabulate (either (index a) (index b))


deMorganDisj :: Representable k => (k a, k b) -> k (Either a b)
deMorganDisj = uncurry (<!!>)
