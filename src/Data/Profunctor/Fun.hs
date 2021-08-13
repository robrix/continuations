module Data.Profunctor.Fun
( -- * CPS functions
  Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
  -- ** Construction
, fun
) where

import           Control.Arrow
import qualified Control.Category as Cat
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
  left'  f = fun (\ k -> either (getFun f (contramap Left k) !) (contramap Right k !))
  right' f = fun (\ k -> either (contramap Left k !) (getFun f (contramap Right k) !))

instance Cochoice (Fun r) where
  unleft  f = fun (\ k -> let f' = (getFun f (K (either (k !) (f' . Right))) !) in f' . Left)
  unright f = fun (\ k -> let f' = (getFun f (K (either (f' . Left) (k !))) !) in f' . Right)

instance Strong (Fun r) where
  first'  f = fun (\ k (a, c) -> getFun f (contramap (,c) k) ! a)
  second' f = fun (\ k (c, a) -> getFun f (contramap (c,) k) ! a)

instance Traversing (Fun r) where
  wander traverse f = fun (\ b a -> getFun (traverse (\ a -> fun (\ k _ -> getFun f k ! a)) a) b ! ())

instance Functor (Fun r a) where
  fmap = rmap
  (<$) = rmap . const

instance Applicative (Fun r x) where
  pure a = Fun (\ k -> K (const (k ! a)))
  f <*> a = fun (\ k x -> getFun f (K (\ f -> getFun a (contramap f k) ! x)) ! x)

instance Monad (Fun r a) where
  m >>= f = fun (\ k x -> getFun m (K (\ a -> getFun (f a) k ! x)) ! x)

instance Arrow (Fun r) where
  arr = Fun . contramap
  first = first'
  second = second'

instance ArrowChoice (Fun r) where
  left = left'
  right = right'

instance ArrowApply (Fun r) where
  app = fun (\ k (f, a) -> getFun f k ! a)


-- Mixfix syntax

type a ~~r = Fun r a
type r~> b = r b

infixr 1 ~~
infixr 0 ~>


-- Construction

fun :: (r ! b -> a -> r) -> Fun r a b
fun = Fun . fmap K
