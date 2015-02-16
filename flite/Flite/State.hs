module Flite.State where

import Control.Monad       (liftM, ap)
import Control.Applicative (Applicative(..))

newtype State s a = S { runState :: s -> (s, a) }

instance Monad (State s) where
  return a = S (\s -> (s, a))
  m >>= f = S (\s -> case runState m s of
                       (s', a) -> runState (f a) s')

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap
