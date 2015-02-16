module Flite.Identity where

import Control.Monad       (liftM, ap)
import Control.Applicative (Applicative(..))

newtype Identity a = I { runIdentity :: a }

instance Monad Identity where
  return a = I a
  I a >>= f = f a

instance Functor Identity where
  fmap = liftM

instance Applicative Identity where
  pure  = return
  (<*>) = ap
