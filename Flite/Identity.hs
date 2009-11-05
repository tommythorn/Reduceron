module Flite.Identity where

newtype Identity a = I { runIdentity :: a }

instance Monad Identity where
  return a = I a
  I a >>= f = f a
