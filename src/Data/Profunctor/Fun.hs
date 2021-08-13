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
) where

import Data.Profunctor.Fun.Internal
