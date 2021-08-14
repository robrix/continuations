module Data.Bifunctor.Conjunction
( exlK
, exrK
, deMorganPair
) where

import Data.Functor.Continuation

exlK :: Contravariant k => k a -> k (a, b)
exlK = contramap fst

exrK :: Contravariant k => k b -> k (a, b)
exrK = contramap snd


deMorganPair :: Contravariant k => Either (k a) (k b) -> k (a, b)
deMorganPair = contramap fst `either` contramap snd
