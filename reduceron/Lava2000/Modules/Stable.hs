module Stable where

import Signal
import Operators
import Generic
import Sequent
import Ref
  
import MyST
  ( STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , runST
  , unsafeInterleaveST
  )

import List
  ( isPrefixOf
  )

----------------------------------------------------------------
-- stable analysis

stable :: Generic a => a -> Signal Bool
stable inp =
  runST
  ( do table     <- tableST
       stableRef <- newSTRef []
       
       let gather (Symbol sym) =
             do ms <- findST table sym
                case ms of
                  Just () -> do return ()
                  Nothing -> do extendST table sym ()
                                mmap gather (deref sym)
                                define (Symbol sym) (deref sym)
    
           define out (DelayBool _ inn) =
             do addStable (out <==> inn)
             
           define out (DelayInt _ inn) =
             do addStable (out <==> inn)
             
           define _ _ =
             do return ()

           addStable x =
             do stables <- readSTRef stableRef
                writeSTRef stableRef (x:stables)

        in mmap gather (struct inp)
       
       stables <- readSTRef stableRef
       return (andl stables)
  )

----------------------------------------------------------------
-- the end.

