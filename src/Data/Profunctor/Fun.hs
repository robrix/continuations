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

import           Control.Arrow
import qualified Control.Category as Cat
import           Data.Bifunctor.Disjunction
import           Data.Functor.Continuation
import           Data.Profunctor
import           Data.Profunctor.Traversing

-- CPS functions

newtype Fun r a b = Fun { getFun :: r ! b -> r ! a }

instance Cat.Category (Fun r) where
  id = Fun id
  f . g = Fun (getFun g . getFun f)

instance Profunctor (Fun r) where
  dimap f g = Fun . dimap (contramap g) (contramap f) . getFun
  lmap f = Fun . rmap (contramap f) . getFun
  rmap g = Fun . lmap (contramap g) . getFun

instance Choice (Fun r) where
  left'  f = Fun (\ k -> (inlK k # f) <!!> inrK k)
  right' f = Fun (\ k -> inlK k <!!> (inrK k # f))

instance Cochoice (Fun r) where
  unleft  f = Fun (\ k -> let f' = (k <!!> inrK f') # f in inlK f')
  unright f = Fun (\ k -> let f' = (inlK f' <!!> k) # f in inrK f')

instance Strong (Fun r) where
  first'  f = Fun (\ k -> K (\ (a, c) -> contramap (,c) k # f ! a))
  second' f = Fun (\ k -> K (\ (c, a) -> contramap (c,) k # f ! a))

instance Traversing (Fun r) where
  wander traverse f = Fun (\ b -> K (\ a -> b # traverse (\ a -> Fun (\ k -> K (\ _ -> k # f ! a))) a ! ()))

instance Functor (Fun r a) where
  fmap = rmap
  (<$) = rmap . const

instance Applicative (Fun r x) where
  pure = Fun . (>$)
  f <*> a = Fun (\ k -> K (\ x -> K (\ f -> contramap f k # a ! x) # f ! x))

instance Monad (Fun r a) where
  m >>= f = Fun (\ k -> K (\ x -> K (\ a -> k # f a ! x) # m ! x))

instance Arrow (Fun r) where
  arr = Fun . contramap
  first = first'
  second = second'

instance ArrowChoice (Fun r) where
  left = left'
  right = right'

instance ArrowApply (Fun r) where
  app = Fun (K . uncurry . fmap (!) . (#))


-- Mixfix syntax

type a ~~r = Fun r a
type r~> b = r b

infixr 1 ~~
infixr 0 ~>


-- Construction

fun :: (r ! b -> a -> r) -> a ~~r~> b
fun = Fun . fmap K


-- Elimination

(#) :: (r ! b) -> (a ~~r~> b) -> (r ! a)
(#) = flip getFun

infixl 7 #

elimFun :: r ! b -> a -> r ! (a ~~r~> b)
elimFun b a = K (\ f -> b # f ! a)
