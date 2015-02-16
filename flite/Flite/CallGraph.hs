module Flite.CallGraph (CallGraph, callReachableGraph, reachable) where

import Flite.Syntax
import Flite.Traversals
import Data.List
import Flite.Dependency (DepGraph, closure, depends)

type CallGraph = DepGraph

-- For each function, determine all its call-reachable functions.
callReachableGraph :: Prog -> CallGraph
callReachableGraph p = closure (zip fs cs)
  where
    fs = map funcName p
    cs = map (nub . calls . funcRhs) p

reachable :: CallGraph -> Id -> [Id]
reachable = depends
