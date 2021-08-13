module Data.Profunctor.Fun
( -- * CPS functions
  Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
) where

import qualified Control.Category as Cat
import           Data.Functor.Continuation
import           Data.Profunctor

-- CPS functions

newtype Fun r a b = Fun { getFun :: r ! b -> r ! a }

instance Cat.Category (Fun r) where
  id = Fun id
  f . g = Fun (getFun g . getFun f)

instance Profunctor (Fun r) where
  dimap f g = Fun . dimap (contramap g) (contramap f) . getFun
  lmap f = Fun . rmap (contramap f) . getFun
  rmap g = Fun . lmap (contramap g) . getFun


-- Mixfix syntax

type a ~~r = Fun r a
type r~> b = r b

infixr 1 ~~
infixr 0 ~>