module Data.Profunctor.Cofun
( -- * Co-functions
  Cofun(..)
  -- * Construction
, (>-)
) where

import Data.Functor.Continuation

-- Co-functions

data Cofun r b a = (:>-) { coreturn :: r ! b, coconst :: a }

infixr 0 :>-


-- Construction

(>-) :: (r ! b) -> a -> Cofun r b a
(>-) = (:>-)

infixr 0 >-
