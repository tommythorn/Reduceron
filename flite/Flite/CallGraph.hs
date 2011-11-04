module Flite.CallGraph (CallGraph, callReachableGraph, reachable) where

import Flite.Syntax
import Flite.Traversals
import Data.List

type CallGraph = [(Id, [Id])]

-- For each function, determine all its call-reachable functions.
callReachableGraph :: Prog -> CallGraph
callReachableGraph p = fixPoint step (zip fs cs)
  where
    fs = map funcName p
    cs = map (nub . calls . funcRhs) p

reachable :: CallGraph -> Id -> [Id]
reachable g f = case lookup f g of { Nothing -> [] ; Just gs -> gs }

step :: CallGraph -> Maybe CallGraph
step g
  | any snd joined = Just (map fst joined)
  | otherwise = Nothing
  where joined = map (join g) g

join :: CallGraph -> (Id, [Id]) -> ((Id, [Id]), Bool)
join g (f, fs) = ((f, reached), length fs < length reached)
  where reached = nub (fs ++ concatMap (reachable g) fs)

fixPoint :: (a -> Maybe a) -> a -> a
fixPoint f a = case f a of { Nothing -> a ; Just b -> fixPoint f b }
