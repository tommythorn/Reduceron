module UnsafeCoerce
  ( unsafeCoerce
  )
 where

import Data.IORef
import System.IO.Unsafe

unsafeCoerce :: a -> b
unsafeCoerce a = unsafePerformIO $
  do writeIORef ref a
     readIORef ref
 where
  ref = unsafePerformIO $
    do newIORef undefined
