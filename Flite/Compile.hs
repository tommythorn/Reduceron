module Flite.Compile (compile) where

import Flite.Syntax
import Flite.Flatten
import Flite.CompileFrontend
import Flite.CompileBackend
import Data.List
import Flite.Inline

compile :: InlineFlag -> Prog -> String
compile i p = program (addBool cs, p2)
  where
    p0 = frontend i p
    p1 = [(f, map getVar args, flatten rhs) | Func f args rhs <- p0]
    cs = nub $ concat [ctrs b | (_, _, bs) <- p1, b <- map snd bs]
    p2 = [ (funId f, length vs, [(v, map (toNode f p1) a) | (v, a) <- bs])
         | (f, vs, bs) <- p1 ]

addBool cs =
  insertIf (notDefined "False") false
    (insertIf (notDefined "True") true cs)
  where
    false = ("False", 0, 0)
    true = ("True", 0, 1)
    notDefined f cs = null [c | (c, _, _) <- cs, c == f]
    insertIf p x xs = if p xs then x:xs else xs

toNode f p (Fun g) = 
  case arities of
    [] -> FUN 2 (funId g)
    n:_ -> FUN n (funId g)
  where arities = [length args | (h, args, rhs) <- p, g == h]
toNode f p (Var v) =
  case v `elemIndex` args of
    Nothing -> VAR v
    Just i -> ARG i
  where args = head [args | (g, args, rhs) <- p, f == g]
toNode f p (Ctr c n i) = FUN (n+1) (funId c)
toNode f p (Alts fs _) = FUN 0 (funId $ head fs)
toNode f p (Int i) = INT i
toNode f p Bottom = FUN 0 "_|_"

funId f | '#' `elem` f = "ALT_" ++ map (tr '#' '_') f
        | otherwise = f

tr :: Eq a => a -> a -> a -> a
tr a b x = if a == x then b else x


ctr :: Exp -> [Cons]
ctr (Ctr c n i) = [(funId c, n, i)]
ctr _ = []

ctrs :: [Exp] -> [Cons]
ctrs = concatMap ctr

getVar :: Exp -> String
getVar (Var v) = v
