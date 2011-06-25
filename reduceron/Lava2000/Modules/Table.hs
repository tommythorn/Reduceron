module Table
  ( table
  , tableProp
  )
 where

import Signal
import Netlist
import Generic
import Sequent
import Property
import MyST

{-

Example use:

Suppose we have a circuit:

  f :: (Signal Bool, Signal Bool) -> Signal Bool
  f (x,y) = and2 (y,x)

Then we can look at its internal structure as follows:

  table (struct (f (var "i")))
  
will evaluate to
  
  ([(1,And 2 3),(3,Var "i_1"),(2,Var "i_2")],Object 1)

For circuits taking more complicated structures as arguments
(such as lists), one has to instantiate the list size first
of course.

If you do not like the "Object 1" output, you can just use
the function `flatten' to get a list of identifiers.

-}

----------------------------------------------------------------
-- table

tableProp :: Checkable a => a -> IO ([(Int,S Int)], [Int])
tableProp a =
  do (props,_) <- properties a
     let (tab, ps) = table (struct props)
     return (tab, flatten ps)

table :: Sequent f => f Symbol -> ([(Int,S Int)], f Int)
table str =
  runST
  ( do ref   <- newSTRef 0
       table <- newSTRef []

       let new =
             do n <- readSTRef ref
                let n' = n+1
                writeSTRef ref n'
                return n'

           define v def =
             do tab <- readSTRef table
                writeSTRef table ((v,def):tab)

       str' <- netlistST new define str
       tab  <- readSTRef table
       return (tab, str')
  )

----------------------------------------------------------------
-- the end.

