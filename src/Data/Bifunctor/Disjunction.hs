module Data.Bifunctor.Disjunction
( -- * Disjunctions
  inlK
, inrK
, (<!!>)
) where

import Data.Functor.Continuation

-- Disjunctions

inlK :: Contravariant k => k (Either a b) -> k a
inlK = contramap Left

inrK :: Contravariant k => k (Either a b) -> k b
inrK = contramap Right

(<!!>) :: Representable k => k a -> k b -> k (Either a b)
(<!!>) = curry deMorganEither

infixr 3 <!!>
