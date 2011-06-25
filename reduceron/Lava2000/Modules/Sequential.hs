module Sequential
  ( simulateSeq
  )
 where

import Ref
import Signal
import Netlist
import Sequent
import Generic

import MyST
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , unsafeInterleaveST
  , runST
  )

----------------------------------------------------------------
-- wire datatype

type Var s
  = (STRef s (S Symbol), STRef s (Wire s))

data Wire s
  = Wire
    { dependencies :: [Var s]
    , kick         :: ST s ()
    }

----------------------------------------------------------------
-- simulate

simulateSeq :: (Generic a, Generic b) => (a -> b) -> [a] -> [b]
simulateSeq circ []   = []
simulateSeq circ inps = runST (
  do roots <- newSTRef []

     let root r =
           do rs <- readSTRef roots
              writeSTRef roots (r:rs)

         new =
           do rval <- newSTRef (error "val?")
              rwir <- newSTRef (error "wire?")
              return (rval, rwir)

         define r s =
           case s of
             Fde _ s s' s'' -> delay' s s' s''
             DelayBool s s' -> delay s s'
             DelayInt  s s' -> delay s s'
             _ ->
               do relate r (arguments s) $
                    eval `fmap` mmap (readSTRef . fst) s
          where
           delay ri@(rinit,_) r1@(pre,_) =
               do state <- newSTRef Nothing
                  r2 <- new
                  root r2

                  relate r [ri] $
                    do ms <- readSTRef state
                       case ms of
                         Just s  -> return s
                         Nothing ->
                           do s <- readSTRef rinit
                              writeSTRef state (Just s)
                              return s

                  relate r2 [r,r1] $
                    do s <- readSTRef pre
                       writeSTRef state (Just s)
                       return s

           -- I have little idea what is going on here, but the
           -- following appears to allow delay with
           -- an enable line
           delay' ri@(rinit,_) ren@(en, _) r1@(pre,_) =
               do state <- newSTRef Nothing
                  r2 <- new
                  root r2

                  relate r [ri] $
                    do ms <- readSTRef state
                       case ms of
                         Just s  -> return s
                         Nothing ->
                           do s <- readSTRef rinit
                              writeSTRef state (Just s)
                              return s

                  relate r2 [r,ren,r1] $
                    do s <- readSTRef pre
                       s' <- readSTRef en
                       case s' of
                         (Bool True) -> do writeSTRef state (Just s)
                         otherwise -> return ()
                       return s

     sr   <- netlistST new define (struct (circ (input inps)))
     rs   <- readSTRef roots
     step <- drive (flatten sr ++ rs)

     outs <- lazyloop $
       do step
          s <- mmap (fmap symbol . readSTRef . fst) sr
          return (construct s)

     let res = takes inps outs
     return res
  )

-- evaluation order

relate :: Var s -> [Var s] -> ST s (S Symbol) -> ST s ()
relate (rval, rwir) rs f =
  do writeSTRef rwir $ 
       Wire{ dependencies = rs
           , kick = do b <- f
                       writeSTRef rval b
           }

drive :: [Var s] -> ST s (ST s ())
drive [] =
  do return (return ())

drive ((rval,rwir):rs) =
  do wire <- readSTRef rwir
     writeSTRef rwir (error "detected combinational loop")
     driv1 <- drive (dependencies wire)
     writeSTRef rwir $
       Wire { dependencies = [], kick = return () }
     driv2 <- drive rs
     return $
       do driv1
          kick wire
          driv2

----------------------------------------------------------------
-- helper functions

lazyloop :: ST s a -> ST s [a]
lazyloop m = 
  do a  <- m
     as <- unsafeInterleaveST (lazyloop m)
     return (a:as)

input :: Generic a => [a] -> a
input xs = out
 where
  out = foldr delay out xs

takes :: [a] -> [b] -> [b]
takes []     _      = []
takes (_:xs) (y:ys) = y : takes xs ys

----------------------------------------------------------------
-- the end.

