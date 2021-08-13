module Data.Function.CPS
( -- * CPS functions
  Fun(..)
) where

import Data.Functor.Continuation

newtype Fun r a b = Fun { getFun :: r ! b -> r ! a }
