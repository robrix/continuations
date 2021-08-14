module Data.Profunctor.Cofun
( -- * Co-functions
  Cofun(..)
  -- * Construction
, (>-)
  -- * Elimination
, withCofun
) where

import Data.Functor.Continuation

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
