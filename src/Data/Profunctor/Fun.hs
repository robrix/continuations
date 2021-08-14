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
  -- * Co-functions
, Cofun(..)
  -- * Mixfix syntax
, type (>-)
, type (-~)
  -- * Construction
, (>-)
  -- * Elimination
, withCofun
, elimCofun
, runCofun
  -- * Computation
, cocurry
, uncocurry
, coap
) where

import           Control.Arrow
import qualified Control.Category as Cat
import           Data.Bifunctor.Disjunction
import           Data.Functor.Continuation
import           Data.Functor.Contravariant
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
  first'  f = fun (\ k (a, c) -> contramap (,c) k # f ! a)
  second' f = fun (\ k (c, a) -> contramap (c,) k # f ! a)

instance Traversing (Fun r) where
  wander traverse f = fun (\ b a -> b # traverse (\ a -> fun (\ k _ -> k # f ! a)) a ! ())

instance Functor (Fun r a) where
  fmap = rmap
  (<$) = rmap . const

instance Applicative (Fun r x) where
  pure = Fun . (>$)
  f <*> a = fun (\ k x -> K (\ f -> contramap f k # a ! x) # f ! x)

instance Monad (Fun r a) where
  m >>= f = fun (\ k x -> K (\ a -> k # f a ! x) # m ! x)

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

elimFun :: (b >-r-~ a) -> r ! (a ~~r~> b)
elimFun (b :>- a) = K (\ f -> b # f ! a)


-- Co-functions

data Cofun r b a = (:>-) { coreturn :: r ! b, coconst :: a }

infixr 0 :>-

instance Profunctor (Cofun r) where
  dimap f g (b :>- a) = contramap f b >- g a
  lmap f (b :>- a) = contramap f b >- a
  rmap g (b :>- a) = b >- g a

instance Functor (Cofun r b) where
  fmap = rmap


-- Mixfix syntax

type a >-r = Cofun r a
type r-~ b = r b

infixr 1 >-
infixr 0 -~


-- Construction

(>-) :: (r ! b) -> a -> (b >-r-~ a)
(>-) = (:>-)


-- Elimination

withCofun :: (b >-r-~ a) -> s ! ((r ! b) -> (s ! a))
withCofun (b :>- a) = K (\ f -> f b ! a)

elimCofun :: (b ~~r~> a) -> r ! (a >-r-~ b)
elimCofun f = K (\ (a :>- b) -> a # f ! b)

runCofun :: (b -> a) -> r ! (a >-r-~ b)
runCofun f = K (\ (a :>- b) -> a ! f b)


-- Computation

cocurry :: Disj d => (c -> a `d` b) -> (b >-r-~ c) ~~r~> a
cocurry f = fun (\ k (b :>- c) -> (k <!!> b) ! f c)

uncocurry :: Disj d => ((b >-r-~ c) ~~r~> a) -> (c ~~r~> (a `d` b))
uncocurry f = fun (\ k c -> inlK k # f ! (inrK k >- c))

coap :: Disj d => c ~~r~> ((b >-r-~ c) `d` b)
coap = Fun (\ k -> (inrK k >-) >$< inlK k)
