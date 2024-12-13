module Flite.WriterState where

import Control.Monad
import Control.Applicative (Applicative(..))

newtype WriterState w s a = WS { runWS :: s -> (s, [w], a) }

instance Monad (WriterState w s) where
  m >>= f = WS $ \s -> let (s0, w0, a) = runWS m s
                           (s1, w1, b) = runWS (f a) s0
                       in  (s1, w0 ++ w1, b)


instance Functor (WriterState w s) where
  fmap = liftM

instance Applicative (WriterState w s) where
  pure a = WS $ \s -> (s, [], a)
  (<*>) = ap

write :: w -> WriterState w s ()
write w = WS $ \s -> (s, [w], ())

get :: WriterState w s s
get = WS $ \s -> (s, [], s)

set :: s -> WriterState w s ()
set s = WS $ \_ -> (s, [], ())
