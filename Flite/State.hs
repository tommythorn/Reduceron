module Flite.State where

newtype State s a = S { runState :: s -> (s, a) }

instance Monad (State s) where
  return a = S (\s -> (s, a))
  m >>= f = S (\s -> case runState m s of
                       (s', a) -> runState (f a) s')
