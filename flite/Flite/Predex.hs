-- Compilation routines for speculative evaluation of primitive redexes

-- Currently we only allow as many primitive applications in a
-- function body as there are PRS registers in the machine.  This is
-- overly-strong constraint that can be generalised (and lifted) in a
-- range of ways with varying ease and efficiency.  (As the feature is
-- experimental, we have taken a rather simple-minded approach to
-- compilation.)

module Flite.Predex where

import Data.List
import Flite.Syntax
import Control.Monad
import Flite.Traversals
import qualified Flite.RedSyntax as R

-- Identify candidates.
identifyPredexCandidates :: Int -> Prog -> Prog
identifyPredexCandidates nregs p = onExp (identify nregs) p

identify :: Int -> Exp -> Exp
identify 0 e = e
identify nregs e =
  case runCount (ident True [] e) nregs of
    (n, e') -> if n == 0 then e' else identify (nregs-n) e'

ident :: Bool -> [(Id, Bool)] -> Exp -> Count Exp
ident False scope (App (Fun f) xs) | isPredexId f =
  do xs' <- mapM (ident False scope) xs
     let e' = App (Fun f) xs'
     if checkArgs scope xs' then one (PrimApp f xs') e' else return e'
ident spine scope (App e es) =
  return App `ap` ident spine scope e `ap` mapM (ident False scope) es
ident spine scope (Let bs e) =
  do let (vs, es) = unzip bs
     let scope' = zip vs (map isPrimApp es) ++ scope
     es' <- mapM (ident False scope') (es)
     e' <- ident spine scope' e
     return (Let (zip vs es') e')
ident spine scope (PrimApp p es) = return (PrimApp p es)
ident spine scope e = return e
 
isPrimApp :: Exp -> Bool
isPrimApp (PrimApp p es) = True
isPrimApp _ = False

checkArgs :: [(Id, Bool)] -> [Exp] -> Bool
checkArgs scope es = all (checkArg scope) es

checkArg :: [(Id, Bool)] -> Exp -> Bool
checkArg scope (Int i) = True
checkArg scope (PrimApp p xs) = True
checkArg scope (Var v) = 
  case lookup v scope of
    Nothing -> True
    Just b -> b
checkArg scope e = False

-- A monad that allows one to count and bound the number of
-- transformations that are applied during a computation.
data Count a = Count { runCount :: Int -> (Int, a) }

instance Monad Count where
  return a = Count $ \n -> (n, a)
  x >>= f = Count $ \n -> case runCount x n of (m, y) -> runCount (f y) m

one :: a -> a -> Count a
one a b = Count $ \n -> if n > 0 then (n-1, a) else (n, b)

-- Given an expression, ensure that a primitive redex candidate does
-- not occupy the spine
removePredexSpine :: Exp -> Exp
removePredexSpine (PrimApp p xs) = App (PrimApp p xs) []
removePredexSpine (Let bs e) = Let bs (removePredexSpine e)
removePredexSpine e = e

-- Given a flattened body, ensure primitive applications occur
-- before their use, and before any non primitive applications.
predexReorder :: Int -> [(Id, App)] -> [(Id, App)]
predexReorder 0 apps = apps
predexReorder maxRegs apps
  | length prims > maxRegs =
      error "Predex: too many primitive applications in body"
  | otherwise = concat (groupApps prims) ++ nonPrims
  where
    (prims, nonPrims) = partition (isPrimitiveApp . snd) apps

-- Detect primitive applications
isPrimitiveApp :: App -> Bool
isPrimitiveApp (Prim p:args) = True
isPrimitiveApp app = False

-- An application A depends on an application B if A refers to B's result.
depends :: (Id, App) -> (Id, App) -> Bool
depends (v, a) (w, b) = any (`refersTo` w) a

refersTo (Var v) w = v == w
refersTo _ _ = False

-- Split applications into groups of independent applications, where
-- each group has no dependencies on any later level.
groupApps :: [(Id, App)] -> [[(Id, App)]]
groupApps = levels depends

levels :: (a -> a -> Bool) -> [a] -> [[a]]
levels p [] = []
levels p xs = this : levels p rest
  where
    this = [x | x <- xs, not (any (p x) xs)]
    rest = [x | x <- xs, any (p x) xs]

-- Associate every primitive application with a register.  Redirect
-- all references to a primitive application to its register.
predex :: Int -> ([R.Atom], [R.App]) -> ([R.Atom], [R.App])
predex 0 (spine, apps) = (spine, apps)
predex n (spine, apps) =
  (map (redirect nprims) spine, map (redirectApp nprims) apps')
  where
    apps' = regAlloc apps
    nprims = countPrims apps'

redirectApp :: Int -> R.App -> R.App
redirectApp n app = mapAtoms (redirect n) app

redirect n (R.VAR s i) | i < n = R.REG s i
redirect n a = a

regAlloc :: [R.App] -> [R.App]
regAlloc = snd . mapAccumL alloc 0

alloc :: Int -> R.App -> (Int, R.App)
alloc r (R.PRIM _ xs) = (r+1, R.PRIM r xs)
alloc r app = (r, app)

countPrims :: [R.App] -> Int
countPrims = sum . map count
  where
    count (R.PRIM r as) = 1
    count _ = 0

mapAtoms :: (R.Atom -> R.Atom) -> R.App -> R.App
mapAtoms f (R.APP n as) = R.APP n (map f as)
mapAtoms f (R.PRIM r as) = R.PRIM r (map f as)
mapAtoms f (R.CASE lut as) = R.CASE lut (map f as)

-- Given a list of applications, return the initial portion that can
-- be executed in the same clock-cycle, and the rest.
splitPredexes :: [R.App] -> ([R.App], [R.App])
splitPredexes apps
  | null apps0 = (apps1, [])
  | otherwise = (apps2, apps3 ++ apps1)
  where
    (apps0, apps1) = span isPRIM apps
    (apps2, apps3) = split [] apps0

    split rs [] = ([], [])
    split rs apps@(R.PRIM r as:rest)
      | any (`refersTo` rs) as = ([], R.PRIM r as:rest)
      | otherwise = (R.PRIM r as:xs, ys)
      where (xs, ys) = split (r:rs) rest

    refersTo (R.REG _ r) rs = r `elem` rs
    refersTo _ rs = False

isPRIM :: R.App -> Bool
isPRIM (R.PRIM r as) = True
isPRIM _ = False
