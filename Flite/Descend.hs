module Flite.Descend where

import Control.Monad
import Flite.Identity
import Flite.Writer

class Descend a where
  descendM :: Monad m => (a -> m a) -> a -> m a

descend :: Descend a => (a -> a) -> a -> a
descend f a = runIdentity (descendM (return . f) a)

extract :: Descend a => (a -> [b]) -> a -> [b]
extract f = fst . runWriter . descendM (\a -> writeMany (f a) >> return a)

universe :: Descend a => a -> [a]
universe a = a : extract universe a
