module Sequent where

----------------------------------------------------------------
-- Sequent (monadic Functor)

class Functor s => Sequent s where
  sequent :: Monad m => s (m a) -> m (s a)

instance Sequent [] where
  sequent = sequence

mmap :: (Monad m, Sequent s) => (a -> m b) -> s a -> m (s b)
mmap f = sequent . fmap f

----------------------------------------------------------------
-- the end.


