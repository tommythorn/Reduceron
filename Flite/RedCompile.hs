module Flite.RedCompile where

-- Parameterise app-length, spine-length and num apps per template,
-- but not arity limit (for now).

import Flite.Syntax
import Flite.Flatten
import Flite.RedFrontend
import Data.List
import Flite.Traversals
import Flite.WriterState
import Flite.Inline
import Flite.Predex
import qualified Flite.RedSyntax as R

import Flite.Pretty
import Debug.Trace

-- Splits applications so that they contain no more than one 'Alts' node.

splitCase :: App -> Bind App
splitCase app
  | length is <= 1 = return app
  | otherwise = do i <- freshId ; write (i, app0) ; splitCase (Var i:rest)
  where
    is = findIndices isAlts app
    (app0, rest) = splitAt (is !! 1) app

-- Splits an application so that it has maximum length 'n'.

splitApp :: Int -> App -> Bind App
splitApp n app
  | length app <= n = return app
  | otherwise = do i <- freshId ; write (i, app0) ; splitApp n (Var i:rest)
  where (app0, rest) = splitAt n app

-- Splits a group of applications so that they each have maximum
-- length 'n' and no more than one 'Alts' node.

splitApps :: Int -> [(Id, App)] -> [(Id, App)]
splitApps n apps = cs ++ ds
  where
    (i, as, bs) = runWS (mapM splitCase' apps) 0
    (j, cs, ds) = runWS (mapM splitApp' (as ++ bs)) i
    splitCase' (v, app) = (,) v `fmap` (splitCase app)
    splitApp' (v, app) = (,) v `fmap` (splitApp n app)

splitSpine :: Int -> [(Id, App)] -> (App, [(Id, App)], [Exp])
splitSpine n ((v, app):rest)
  | length spine <= n = (spine, rest, luts)
  | otherwise = -- Needed????
      ( Var v:takeBack (n-1) spine
      , (v, dropBack (n-1) spine):rest
      , luts
      )
  where
    spine = filter (not . isAlts) app
    luts = filter isAlts app

-- Translates a program to Reduceron syntax.  Takes the max
-- application length and max spine length as arguments.

translate :: InlineFlag -> Int -> Int -> Int -> Prog -> R.Prog
translate i n m nregs p = map (trDefn n m nregs p2) p2
  where
    p0 = frontend nregs i (force01:force0:force1:p)
    p1 = [ (f, map getVar args, flatten $ removePredexSpine rhs)
         | Func f args rhs <- p0
         ]
    p2 = lift "main" p1

trDefn n m nregs p (f, args, xs) =
  (f, length args, luts, pushs', apps')
  where
    (spine, body, ls) = splitSpine m xs
    body' = predexReorder nregs $ splitApps n body
    d = (f, args, spine, body')
    luts = map (indexOf p) $ map getAlts ls
    apps = map (trApp p d . snd) body'
    pushs = map (tr p d) $ filter (not . isAlts) spine
    (pushs', apps') = predex nregs (pushs, apps)

trApp p d app
   | isPrimitiveApp app = R.PRIM (-1) rest
  -- - | isPrimitiveApp app = R.PRIM (-1) (reverse rest) {- PV STACK -}
  | null luts = R.APP (isNormal rest) rest
  | otherwise = R.CASE (head luts) rest
  where
    app' = force app
    --app' = app {- PV STACK -}
    luts = map (indexOf p) $ map getAlts $ filter isAlts app'
    rest = map (tr p d) $ filter (not . isAlts) app'

force app@[Prim p,y,Int _] = Fun "!force0" : app
force app@[Prim p,Int i,y] = Fun "!force1" : app
force app
  | isPrimitiveApp app = Fun "!force01" : app
  | otherwise = app

indexOf p f =
  case [i | ((g, args, rhs), i) <- zip p [0..], f == g] of
    [] -> error "RedCompile: indexOf"
    i:_ -> i

isNormal (R.CON n c:rest) = length rest <= n
isNormal (R.FUN b n f:rest) = length rest < n
isNormal _ = False

tr p d (Int i) = R.INT i
tr p d (Prim f) = R.PRI 2 f
tr p d (Fun f) =
  case xs of
    [] -> R.PRI 2 f
    (i, args):_ -> R.FUN False (length args) i
  where xs = [(i, args) | ((g, args, rhs), i) <- zip p [0..], f == g]
tr p (f, args, spine, body) (Var v) =
  case v `elemIndex` args of
    Nothing -> R.VAR shared idx
    Just i -> R.ARG shared i
  where
    shared = (length $ filter (== v)
                     $ concatMap (concatMap vars) (spine : map snd body)) > 1
    idx = case [i | ((w, _), i) <- zip body [0..], v == w] of
            [] -> error ("Unbound variable: " ++ v)
            i:_ -> i
tr p d (Ctr c n i) = R.CON n i
tr p d Bottom = R.INT 0

-- Set boolean 'original' flag on funtions; if true, function was
-- originally defined, and if false, function was introduced in
-- Reduceron compilation process.

flagFuns :: Int -> R.Prog -> R.Prog
flagFuns i p = map flag p
  where
    flag (f, pop, luts, push, apps) =
      (f, pop, luts, map fl push, map (mapAtoms fl) apps)
    fl (R.FUN _ n f) = R.FUN (f < i) n f
    fl a = a

-- Fragment a program such that: (1) each template contains at most
-- 'n' applications; (2) each template contains at most 'm' LUTs; (3)
-- each template pushes a maximum of 'm' atoms; (4) if a template
-- pushes more than one atom, then it contains at most 'n-1'
-- applications; (5) the first atom pushed by the final template does
-- not refer to any of that template's applications (the 'refers
-- check').

fragment :: Int -> Int -> R.Prog -> R.Prog
fragment n m p = flagFuns (length p) (p' ++ ts')
  where
    (_, ts, p') = runWS (mapM (frag n m) p) (length p)
    ts' = map snd (sortBy cmp ts)
    cmp (a, b) (c, d) = compare a c

sub n m = m-n

frag n m (f, pop, luts, push, apps)
  | length apps >= n || any isPRIM apps = fr n m (f, pop, luts, push, apps)
  | length luts > m =
      do x <- newId
         t <- frag n m (f, pop, dropBack m luts, push, apps)
         write (x, t)
         return (f, 0, takeBack m luts, [R.FUN False 0 x], [])
  | refersCheck (head push) = fr n m (f, pop, luts, push, apps)
  | otherwise = return (f, pop, luts, push, apps)

fr n m (f, pop, luts, push, apps) =
  do x <- newId
     let offset = length (take n apps0)
     let apps' = map (relocate (sub offset)) (drop n apps0 ++ apps1)
     let push' = map (reloc (sub offset)) push
     t <- frag n m (f, pop, dropBack m luts, push', apps')
     write (x, t)
     return (f, 0, takeBack m luts, [R.FUN False 0 x], take n apps0)
  where
    (apps0, apps1) = splitPredexes apps

relocate f app = mapAtoms (reloc f) app

reloc f (R.VAR sh i) = R.VAR sh (f i)
reloc f x = x

refersCheck (R.VAR sh i) = i >= 0
refersCheck _ = False

-- Top-level compilation

redCompile :: InlineFlag -> Int -> Int -> Int -> Int -> Int -> Prog -> R.Prog
redCompile i slen alen napps nluts nregs =
  fragment napps nluts . translate i alen slen nregs

-- Auxiliary functions

takeBack n xs = reverse $ take n $ reverse xs

dropBack n xs = reverse $ drop n $ reverse xs

getVar :: Exp -> String
getVar (Var v) = v

vars :: Exp -> [Id]
vars (Var v) = [v]
vars e = []

isAlts :: Exp -> Bool
isAlts (Alts fs n) = True
isAlts e = False

getAlts :: Exp -> Id
getAlts (Alts fs n)
  | null fs = error "RedCompile: getAlts"
  | otherwise = head fs

lift f p = xs ++ ys
  where (xs, ys) = partition (\(g, _, _) -> f == g) p

type Bind a = WriterState (Id, [Exp]) Int a

freshId :: Bind Id
freshId = do n <- get ; set (n+1) ; return ("new_bind_" ++ show n)

type Define a = WriterState (Int, R.Template) Int a

newId :: Define Int
newId = do n <- get ; set (n+1) ; return n
