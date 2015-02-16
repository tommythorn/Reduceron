module Flite.Writer where

import Control.Monad       (liftM, ap)
import Control.Applicative (Applicative(..))

data Writer w a = W [w] a

instance Monad (Writer w) where
  return a = W [] a
  W w0 a0 >>= f = case f a0 of W w1 a1 -> W (w0 ++ w1) a1

instance Functor (Writer w) where
  fmap = liftM

instance Applicative (Writer w) where
  pure  = return
  (<*>) = ap

runWriter :: Writer w a -> ([w], a)
runWriter (W ws a) = (ws, a)

write :: w -> Writer w ()
write w = W [w] ()

writeMany :: [w] -> Writer w ()
writeMany ws = W ws ()
