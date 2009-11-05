module Flite.Strictify
  ( strictifyPrim
  , strictifyPrimWithPVStack
  , forceAndRebind
  ) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.CallGraph
import Data.List
import Flite.LambdaLift

isInt (Int i) = True
isInt _ = False

mkApp f [] = f
mkApp (App f es) fs = App f (es ++ fs)
mkApp f es = App f es

primSatErrMsg :: String
primSatErrMsg = "Applications of primitives must be saturated"

-- Makes sure that arguments to primitive functions are forced before
-- the primitive is applied.
strictifyPrim :: Prog -> Prog
strictifyPrim = onExp prim
  where
    prim (App (Fun f) (a:rest))
      | isUnaryPrim f = mkApp result (map prim rest)
      where a' = prim a
            result = case isInt a' of
                       False -> App a' [Fun f]
                       True  -> App (Fun f) [a']
    prim (App (Fun f) (a:b:rest))
      | isBinaryPrim f = mkApp result (map prim rest)
      where (a', b') = (prim a, prim b)
            result = case (isInt a', isInt b') of
                       (False, False) -> App b' [App a' [Fun f]]
                       (False, True ) -> App a' [Fun f, b']
                       (True , False) -> App b' [App (Fun f) [a']]
                       (True , True ) -> App (Fun f) [a', b']
    prim (App (Fun f) es)
      | isUnaryPrim f || isBinaryPrim f = error primSatErrMsg
    prim (Fun f)
      | isUnaryPrim f || isBinaryPrim f = error primSatErrMsg
    prim e = descend prim e

-- Same as above, except assuming that reduction machine has a
-- special primitive-value (PV) stack available.
strictifyPrimWithPVStack :: Prog -> Prog
strictifyPrimWithPVStack = onExp prim
  where
    prim (App (Fun f) (a:rest))
      | isUnaryPrim f = mkApp result (map prim rest)
      where a' = prim a
            result = catApp [a', Fun f]
    prim (App (Fun f) (a:b:rest))
      | isBinaryPrim f = mkApp result (map prim rest)
      where (a', b') = (prim a, prim b)
            result = catApp [b', a', Fun f]
    prim (App (Fun f) es)
      | isUnaryPrim f || isBinaryPrim f = error primSatErrMsg
    prim (Fun f)
      | isUnaryPrim f || isBinaryPrim f = error primSatErrMsg
    prim e = descend prim e

catApp :: [Exp] -> Exp
catApp es = App x xs
  where
    x:xs = concatMap contents es
    contents (App e es) = e:es
    contents e = [e]

{-

Attempts to rebind strictly-needed variables (of type integer) so
their evaluated forms can be viewed as unboxed integers.  The aim is
to increase the scope of PRS.  The transformation proceeds as follows:

STEP 1. Look for functions of the form

  f ... = ... case p e1 e2 of { False -> alt1 ; True -> alt2 } ...

where p is a primitive function strict in both arguments returning a
boolean, and alt1 or alt2 can lead to another call of f.

STEP 2. Take all the variables of type integer referred to in e1 or e2
that also referred to in alt1 or alt2.  Call them v1..vn.  Proceed
only if v1..vn is non-empty.

STEP 3. Abstract the expression of interest into a function h:

  f ... = ... h v1..vn w1..wn ...

  h v1..vn w1..wn = case p e1 e2 of { False -> alt1 ; True -> alt2 };

where w1..wn are the free variables, other than v1..vn, in the
case expression.

STEP 4. Create function f' like f but which forces evaluation of
v1..vn before applying h:

  f' ... = ... vn (..(v1 h)) w1..wn ...

STEP 5. Now calls to f can be replaced by calls to f'.  However, as
primed functions are meant to be wrappers, only calls to f which occur
in a function that is NOT call-reachable from f should be replaced.

-}

forceAndRebind :: Prog -> Prog
forceAndRebind p = map (wrap cg wrapperIds) p ++ wrappers
  where
    cg = callReachableGraph p
    wrappers = lambdaLift $ concatMap (makeWrapper cg) p
    wrapperIds = map funcName wrappers

wrap :: CallGraph -> [Id] -> Decl -> Decl
wrap cg ws (Func f args rhs) = Func f args (wrapExp f cg ws rhs)

wrapExp :: Id -> CallGraph -> [Id] -> Exp -> Exp
wrapExp f cg ws (Fun g)
  | g' `elem` ws && f `notElem` reachable cg g = Fun g'
  | otherwise = Fun g
  where g' = g ++ "_W"
wrapExp f cg ws e = descend (wrapExp f cg ws) e

makeWrapper :: CallGraph -> Decl -> [Decl]
makeWrapper cg (Func f args rhs)
  | rhs == rhs' = []
  | otherwise = [Func f' args rhs']
  where
    rhs' = abstract f cg rhs
    f' = f ++ "_W"

neededVars :: Exp -> [Id]
neededVars (App (Fun p) es)
  | isPrimId p = concatMap neededVars es
neededVars (Var v) = [v]
neededVars _ = []

{-
abstract :: Id -> CallGraph -> Exp -> Exp
abstract f cg (Case subject@(App (Fun p) es) as)
  | isPrimId p
 && not (null vs)
 && recursive = force (reverse vs) (Lam vs (Case (App (Fun p) es) as'))
  where
    nvs = neededVars subject
    fvs = filter (`elem` nvs) $ concatMap (freeVars . snd) as
    vs = dups (nvs ++ fvs)
    recursive = f `elem` concatMap (reachable cg)
                  (concatMap calls (subject:map snd as))
    as' = [(p, abstract f cg e) | (p, e) <- as]
abstract f cg e = descend (abstract f cg) e

force :: [Id] -> Exp -> Exp
force [] e = e
force (v:vs) e = App (Var v) [force vs e]
-}

abstract :: Id -> CallGraph -> Exp -> Exp
abstract f cg (Case subject@(App (Fun p) es) as)
  | isPrimId p
 && not (null vs)
 && recursive = 
  App (force (reverse vs) (Lam (vs ++ ws) (Case (App (Fun p) es) as')))
      (map Var ws)
  where
    nvs = neededVars subject
    fvs = filter (`elem` nvs) $ concatMap (freeVars . snd) as
    vs = dups (nvs ++ fvs)
    recursive = f `elem` concatMap (reachable cg)
                  (concatMap calls (subject:map snd as))
    as' = [(p, abstract f cg e) | (p, e) <- as]
    ws = filter (`notElem` vs) $ nub $ concatMap freeVars $ (es ++ map snd as)
abstract f cg e = descend (abstract f cg) e

force :: [Id] -> Exp -> Exp
force [] e = e
force (v:vs) e = App (Var v) [force vs e]


-- Return elements that occur more than once
dups :: Eq a => [a] -> [a]
dups [] = []
dups (x:xs)
  | x `elem` xs = x : dups (filter (/= x) xs)
  | otherwise = dups xs
