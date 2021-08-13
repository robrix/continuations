{-# LANGUAGE FunctionalDependencies #-}
module Data.Profunctor.Fun.Internal
( -- * CPS functions
  Fun(..)
  -- ** Mixfix syntax
, type (~~)
, type (~>)
  -- ** Construction
, fun
  -- * Contravariant continuation-passing style
, ContravariantCPS(..)
) where

import           Control.Arrow
import qualified Control.Category as Cat
import           Data.Functor.Continuation
import           Data.Profunctor
import           Data.Profunctor.Traversing

-- CPS functions

newtype Fun r a b = Fun { (#) :: r ! b -> r ! a }

infixl 7 #

instance Cat.Category (Fun r) where
  id = Fun id
  f . g = Fun ((g #) . (f #))

instance Profunctor (Fun r) where
  dimap f g = Fun . dimap (contramap g) (contramap f) . (#)
  lmap f = Fun . rmap (contramap f) . (#)
  rmap g = Fun . lmap (contramap g) . (#)

instance Choice (Fun r) where
  left'  f = Fun (\ k -> (f # inlK k) <!!> inrK k)
  right' f = Fun (\ k -> inlK k <!!> (f # inrK k))

instance Cochoice (Fun r) where
  unleft  f = Fun (\ k -> let f' = f # (k <!!> inrK f') in inlK f')
  unright f = Fun (\ k -> let f' = f # (inlK f' <!!> k) in inrK f')

instance Strong (Fun r) where
  first'  f = fun (\ k (a, c) -> f # contramap (,c) k ! a)
  second' f = fun (\ k (c, a) -> f # contramap (c,) k ! a)

instance Traversing (Fun r) where
  wander traverse f = fun (\ b a -> traverse (\ a -> fun (\ k _ -> f # k ! a)) a # b ! ())

instance Functor (Fun r a) where
  fmap = rmap
  (<$) = rmap . const

instance Applicative (Fun r x) where
  pure = Fun . (>$)
  f <*> a = fun (\ k x -> f # K (\ f -> a # contramap f k ! x) ! x)

instance Monad (Fun r a) where
  m >>= f = fun (\ k x -> m # K (\ a -> f a # k ! x) ! x)

instance Arrow (Fun r) where
  arr = Fun . contramap
  first = first'
  second = second'

instance ArrowChoice (Fun r) where
  left = left'
  right = right'

instance ArrowApply (Fun r) where
  app = fun (\ k (f, a) -> f # k ! a)


-- Mixfix syntax

type a ~~r = Fun r a
type r~> b = r b

infixr 1 ~~
infixr 0 ~>


-- Construction

fun :: (r ! b -> a -> r) -> a ~~r~> b
fun = Fun . fmap K


-- Contravariant continuation-passing style

class Contravariant k => ContravariantCPS r k | k -> r where
  (<#>) :: (a' ~~r~> a) -> (k a -> k a')

  infixl 4 <#>
