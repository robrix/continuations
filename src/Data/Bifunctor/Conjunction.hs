module Data.Bifunctor.Conjunction
( -- * Conjunctions
  Conj(..)
, exlF
, exrF
, deMorganConj
) where

import Control.Applicative (liftA2)
import Data.Functor.Continuation

-- Conjunctions

class Conj c where
  (>!!<) :: Applicative f => f a -> f b -> f (a `c` b)
  infixr 3 >!!<
  exl :: Contravariant k => k a -> k (a `c` b)
  exr :: Contravariant k => k b -> k (a `c` b)

instance Conj (,) where
  (>!!<) = liftA2 (,)
  exl = contramap fst
  exr = contramap snd


exlF :: (Functor f, Conj c) => f (a `c` b) -> f a
exlF = fmap (exl (K id) !)

exrF :: (Functor f, Conj c) => f (a `c` b) -> f b
exrF = fmap (exr (K id) !)


deMorganConj :: (Contravariant k, Conj c) => Either (k a) (k b) -> k (a `c` b)
deMorganConj = exl `either` exr
