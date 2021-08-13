module Control.Continuation
( type (!)(..)
) where

newtype r ! a = K { (!) :: a -> r }
