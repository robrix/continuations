module Data.Profunctor.Fun
( -- * CPS functions
  Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
  -- ** Construction
, fun
  -- ** Elimination
, (#)
, elimFun
) where

import Data.Functor.Continuation
import Data.Profunctor.Fun.Internal


-- Construction

fun :: (r ! b -> a -> r) -> a ~~r~> b
fun = Fun . fmap K


-- Elimination

elimFun :: r ! b -> a -> r ! (a ~~r~> b)
elimFun b a = K (\ f -> b # f ! a)
