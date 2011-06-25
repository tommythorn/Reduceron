module Netlist
  ( netlist
  , netlistIO
  , netlistST
  )
 where

import Ref
import Signal
import Generic
import Sequent

import MyST
  ( ST
  )

----------------------------------------------------------------
-- netlist

netlist :: Functor f => (S a -> a) -> f Symbol -> f a
netlist phi symbols = fmap cata symbols
 where
  cata (Symbol sym) = cata' sym
  cata'             = memoRef (phi . fmap cata . deref)

netlistIO :: Sequent f => IO v -> (v -> S v -> IO ()) -> f Symbol -> IO (f v)
netlistIO new define symbols =
  do tab <- tableIO
     
     let gather (Symbol sym) =
           do visited <- findIO tab sym
              case visited of
                Just v  -> do return v
                Nothing -> do v <- new
                              extendIO tab sym v
                              s <- mmap gather (deref sym)
                              define v s
                              return v
           
      in mmap gather symbols

netlistST :: Sequent f => ST s v -> (v -> S v -> ST s ()) -> f Symbol -> ST s (f v)
netlistST new define symbols =
  do tab <- tableST
     
     let gather (Symbol sym) =
           do visited <- findST tab sym
              case visited of
                Just v  -> do return v
                Nothing -> do v <- new
                              extendST tab sym v
                              s <- mmap gather (deref sym)
                              define v s
                              return v
           
      in mmap gather symbols

----------------------------------------------------------------
-- the end.

