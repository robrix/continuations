module Data.Function.CPS
( -- * CPS functions
  Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
) where

import qualified Control.Category as Cat
import           Data.Functor.Continuation

-- CPS functions

newtype Fun r a b = Fun { getFun :: r ! b -> r ! a }

instance Cat.Category (Fun r) where
  id = Fun id
  f . g = Fun (getFun g . getFun f)


-- Mixfix syntax

type a ~~r = Fun r a
type r~> b = r b

infixr 1 ~~
infixr 0 ~>
