module SequentialConstructive
  ( simulateCon
  )
 where

import Ref
import Signal
import Netlist
import Sequent
import Generic
import Error

import Data.IORef
import System.IO.Unsafe

----------------------------------------------------------------
-- wire datatype

type Time
  = IORef ()

data Timed a
  = a `At` Time
  | Uninitialized

data Wire
  = Wire
    { components :: [Component]
    , value      :: Timed (S Symbol)
    }

type Component
  = Time -> IO ()

----------------------------------------------------------------
-- simulate

simulateCon :: (Generic a, Generic b) => (a -> b) -> [a] -> [b]
simulateCon circ inps = unsafePerformIO $
 do micro <- newSet
    macro <- newSet
    time0 <- newIORef ()

    let new =
          do rwire <- newIORef (Wire{ components = [], value = Uninitialized })
             return rwire

        define rwire (DelayBool init next) =
          do delay rwire init next

        define rwire (DelayInt init next) =
          do delay rwire init next

        define rwire sym =
          case arguments sym of
            []   -> addSet macro constant
            args -> sequence_ [ compWire rarg propagate | rarg <- args ]
         where
          propagate time =
            do sym' <- mmap (`valueWire` time) sym
               case evalLazy sym' of
                 Nothing -> return ()
                 Just v  -> updateWire rwire time v

          constant time =
            do propagate time
               addSet macro constant

        delay rwire init next =
          do compWire next nextState
             compWire init initState
         where
          nextState time =
            do mv <- valueWire next time
               case mv of
                 Nothing -> return ()
                 Just v  -> addSet macro (\t -> updateWire rwire t v)

          initState time
            | time == time0 = do mv <- valueWire init time
                                 case mv of
                                   Nothing -> return ()
                                   Just v  -> updateWire rwire time v
            | otherwise     = do return ()

        compWire rwire comp =
          do wire <- readIORef rwire
             writeIORef rwire (wire{ components = comp : components wire })

        valueWire rwire time =
          do wire <- readIORef rwire
             return $
               case value wire of
                 v `At` time'
                   | time == time' -> Just v
                 _                 -> Nothing

        actualValueWire rwire time =
          do mv <- valueWire rwire time
             case mv of
               Just v  -> return v
               Nothing -> wrong Error.UndefinedWire

        updateWire rwire time v =
          do wire <- readIORef rwire
             mv   <- valueWire rwire time
             case mv of
               Just v' | v =/= v'  -> wrong Error.BadCombinationalLoop
                       | otherwise -> return ()

               _ -> do writeIORef rwire (wire{ value = v `At` time })
                       sequence_ [ addSet micro comp | comp <- components wire ]

        Bool b1 =/= Bool b2 = b1 /= b2
        Int  n1 =/= Int  n2 = n1 /= n2
        _       =/= _       = True

    sr <- netlistIO new define (struct (circ (input inps)))

    outs <- timedLazyLoop time0 $ \time ->
      do emptySet macro ($ time)
         while (emptySet micro ($ time))
         s <- mmap (`actualValueWire` time) sr
         return (construct (symbol `fmap` s))

    let res = takes inps outs
    return res

----------------------------------------------------------------
-- helper functions

newSet :: IO (IORef [a])
newSet = newIORef []

addSet :: IORef [a] -> a -> IO ()
addSet rset x =
  do xs <- readIORef rset
     writeIORef rset (x:xs)

emptySet :: IORef [a] -> (a -> IO ()) -> IO Bool
emptySet rset action =
  do xs <- readIORef rset
     writeIORef rset []
     case xs of
       [] -> do return False
       _  -> do sequence [ action x | x <- xs ]
                return True

while :: Monad m => m Bool -> m ()
while m =
  do b <- m
     if b then while m
          else return ()

timedLazyLoop :: Time -> (Time -> IO a) -> IO [a]
timedLazyLoop t m =
  do a  <- m t
     t' <- newIORef ()
     as <- unsafeInterleaveIO (timedLazyLoop t' m)
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

