module Data.Profunctor.Cofun
( -- * Co-functions
  Cofun(..)
) where

import Data.Functor.Continuation

-- Co-functions

data Cofun r b a = (:>-) { coreturn :: r ! b, coconst :: a }

infixr 0 :>-
