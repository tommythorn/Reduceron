module MyST
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , runST
  , fixST
  
  , unsafePerformST
  , unsafeInterleaveST
  , unsafeIOtoST
  )
 where

import System.IO
import System.IO.Unsafe
import Data.IORef

newtype ST s a
  = ST (IO a)

unST :: ST s a -> IO a
unST (ST io) = io

instance Functor (ST s) where
  fmap f (ST io) = ST (fmap f io)

instance Monad (ST s) where
  return a    = ST (return a)
  ST io >>= k = ST (do a <- io ; unST (k a))

newtype STRef s a
  = STRef (IORef a)
  
instance Eq (STRef s a) where
  STRef r1 == STRef r2 = r1 == r2

newSTRef :: a -> ST s (STRef s a)
newSTRef a = ST (STRef `fmap` newIORef a)

readSTRef :: STRef s a -> ST s a
readSTRef (STRef r) = ST (readIORef r)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef r) a = ST (writeIORef r a)

runST :: (forall s . ST s a) -> a
runST st = unsafePerformST st

fixST :: (a -> ST s a) -> ST s a
fixST f = ST (fixIO (unST . f))

unsafePerformST :: ST s a -> a
unsafePerformST (ST io) = unsafePerformIO io

unsafeInterleaveST :: ST s a -> ST s a
unsafeInterleaveST (ST io) = ST (unsafeInterleaveIO io)

unsafeIOtoST :: IO a -> ST s a
unsafeIOtoST = ST

