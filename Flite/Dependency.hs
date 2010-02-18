module Flite.Dependency
  ( DepGraph     -- type DepGraph = [(Id, [Id])]
  , depends      -- :: DepGraph -> Id -> [Id]
  , callGraph    -- :: [Decl] -> DepGraph
  , letGraph     -- :: [Binding] -> DepGraph
  , closure      -- :: DepGraph -> DepGraph
  , callGroups   -- :: [Decl] -> [[Decl]]
  , letGroups    -- :: [Binding] -> [[Binding]]
  ) where

import Flite.Syntax
import Flite.Traversals
import Data.List

-- Depdendency graphs
type DepGraph = [(Id, [Id])]

depends :: DepGraph -> Id -> [Id]
depends g f = case lookup f g of { Nothing -> [] ; Just gs -> gs }

closure :: DepGraph -> DepGraph
closure g = fixPoint step g

step :: DepGraph -> Maybe DepGraph
step g
  | any snd joined = Just (map fst joined)
  | otherwise = Nothing
  where joined = map (join g) g

join :: DepGraph -> (Id, [Id]) -> ((Id, [Id]), Bool)
join g (f, fs) = ((f, reached), length fs < length reached)
  where reached = nub (fs ++ concatMap (depends g) fs)

fixPoint :: (a -> Maybe a) -> a -> a
fixPoint f a = case f a of { Nothing -> a ; Just b -> fixPoint f b }

-- For each function, determine all functions it calls
callGraph :: [Decl] -> DepGraph
callGraph p = (zip fs cs)
  where
    fs = map funcName p
    cs = map (nub . calls . funcRhs) p

-- Strongly connected components, in dependency order
components :: DepGraph -> [[Id]]
components g = order g (comps (closure g) (map fst g))

comps :: DepGraph -> [Id] -> [[Id]]
comps g [] = []
comps g (f:fs)
  | null hs = [f] : comps g fs
  | otherwise = hs : comps g (filter (`notElem` hs) fs)
  where hs = [h | h <- depends g f, f `elem` depends g h]

order :: DepGraph -> [[Id]] -> [[Id]]
order g xs = ord [] xs
  where
    allIds = map fst g

    ord seen [] = []
    ord seen xs = free ++ ord (concat free ++ seen) rest
      where
        (free, rest) = partition p xs
        p ys = all (independent ys) (concatMap (depends g) ys)
        independent ys z = z `elem` ys || z `notElem` allIds || z `elem` seen

-- Group function definitions into recursive call groups
callGroups :: [Decl] -> [[Decl]]
callGroups p = map (map (lookupFunc p)) (components (callGraph p))

-- For each let binding, determine all other let bindings it depends on
letGraph :: [Binding] -> DepGraph
letGraph bs = zip vs (map (filter (`elem` vs) . freeVars) es)
  where (vs, es) = unzip bs

-- Group lets into recursive binding groups
letGroups :: [Binding] -> [[Binding]]
letGroups bs = map (map (lookupBinding bs)) (components (letGraph bs))

lookupBinding :: [(Id, Exp)] -> Id -> (Id, Exp)
lookupBinding bs w = 
  case lookup w bs of
    Nothing -> error "Dependency: lookupBinding"
    Just e -> (w, e)
