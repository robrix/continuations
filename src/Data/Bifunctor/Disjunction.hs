module Data.Bifunctor.Disjunction
( -- * Disjunctions
  Disj(..)
, inlK
, inrK
, deMorganDisj
) where

import Data.Functor.Continuation
import Data.Functor.Identity

-- Disjunctions

class Disj d where
  inl :: Functor f => f a -> f (a `d` b)
  inr :: Functor f => f b -> f (a `d` b)
  (<!!>) :: Representable k => k a -> k b -> k (a `d` b)
  infixr 3 <!!>

instance Disj Either where
  inl = fmap Left
  inr = fmap Right
  a <!!> b = tabulate (either (index a) (index b))


inlK :: (Contravariant k, Disj d) => k (a `d` b) -> k a
inlK = contramap (runIdentity . inl . Identity)

inrK :: (Contravariant k, Disj d) => k (a `d` b) -> k b
inrK = contramap (runIdentity . inr . Identity)


deMorganDisj :: (Representable k, Disj d) => (k a, k b) -> k (a `d` b)
deMorganDisj = uncurry (<!!>)