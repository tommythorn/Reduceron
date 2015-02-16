module Flite.Fresh where

import Control.Monad       (liftM, ap)
import Control.Applicative (Applicative(..))

data Fresh a = Fresh { runFresh :: String -> Int -> (Int, a) }

instance Monad Fresh where
  return a = Fresh (\s i -> (i, a))
  m >>= f  = Fresh (\s i -> case runFresh m s i of
                              (j, a) -> runFresh (f a) s j)

instance Functor Fresh where
  fmap = liftM

instance Applicative Fresh where
  pure  = return
  (<*>) = ap

fresh :: Fresh String
fresh = Fresh (\s i -> (i+1, s ++ show i))
