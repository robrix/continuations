module Data.Profunctor.Fun
( -- * CPS functions
  Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
  -- ** Construction
, fun
  -- ** Elimination
, elimFun
) where

import Data.Functor.Continuation
import Data.Profunctor.Cofun
import Data.Profunctor.Fun.Internal

-- Elimination

elimFun :: (b >-r-~ a) -> r ! (a ~~r~> b)
elimFun (b :>- a) = K (\ f -> f # b ! a)
