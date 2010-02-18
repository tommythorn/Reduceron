module Flite.Strictness
  ( Strictness           -- type Strictness = [(Id, [Bool])]
  , strictnessAnalysis   -- :: Prog -> Strictness
  ) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.Dependency

-- Strictness of each function in each argument
type Strictness = [(Id, [Bool])]

-- Bottom and top of the abstract domain
bottom :: Exp
bottom = Int 0

mayTerminate :: Exp
mayTerminate = Int 1

-- Conjunction and disjunction over abstract domain
conj :: Exp -> Exp -> Exp
conj e0 e1 = App (Fun "&") [e0, e1]

disj :: Exp -> Exp -> Exp
disj e0 e1 = App (Fun "|") [e0, e1]

conjList :: [Exp] -> Exp
conjList = foldr conj mayTerminate

disjList :: [Exp] -> Exp
disjList = foldr disj bottom

-- Lift an expression to the abstract domain
abstr :: Exp -> Exp
abstr Bottom = bottom
abstr (Var v) = Var v
abstr (Fun f) = Fun f
abstr (Int n) = mayTerminate
abstr (Con c) = mayTerminate
abstr (App (Con c) es) = mayTerminate
abstr (App (Fun f) [e0, e1])
  | isPrimId f = conj (abstr e0) (abstr e1)
abstr (App e es) = App (abstr e) (map abstr es)
abstr (Case e alts) = conj e (abstrAlts alts)
abstr (Let bs e) = Let [(v, abstr e) | (v, e) <- bs] (abstr e)

abstrAlts :: [Alt] -> Exp
abstrAlts alts = disjList (map abstr es)
  where es = [ substMany e (zip (repeat mayTerminate) (patVars p))
             | (p, e) <- alts ]

-- Evaluate an abstract expression
eval :: Strictness -> Exp -> Exp
eval p (Int i) = Int i
eval p (Fun f) = apply p f []
eval p (Var v) = Var v
eval p (App (Fun "&") [e0, e1]) = 
  case eval' p e0 of { Int 0 -> Int 0 ; Int 1 -> eval p e1 ; Var v -> Var v }
eval p (App (Fun "|") [e0, e1]) = 
  case eval' p e0 of { Int 0 -> eval p e1 ; Int 1 -> Int 1 ; Var v -> Var v }
eval p (App (Fun "=") [e0, e1]) = 
  case eval' p e0 of { Int 0 -> inv (eval' p e1)
                     ; Int 1 -> eval' p e1
                     ; Var v -> Var v }
eval p (App e es) =
  case eval p e of
    Var v -> Var v
    Int i -> Int i
    Fun f -> apply p f es
    App (Fun f) args -> apply p f (es ++ args)

eval' :: Strictness -> Exp -> Exp
eval' p e = case eval p e of { App e es -> mayTerminate ; e -> e }

inv :: Exp -> Exp
inv (Var v) = Var v
inv (Int 1) = Int 0
inv (Int 0) = Int 1

apply :: Strictness -> Id -> [Exp] -> Exp
apply prog f xs
  | length xs < length params = App (Fun f) xs
  | otherwise = eval prog (conjList [x | (True, x) <- zip params xs])
  where
    params = head [ps | (g, ps) <- prog, g == f]

-- Inline let expressions in an abstract expression.  Looses sharing,
-- and assumes that all cyclic let bindings terminate.
inlineLet :: Exp -> Exp
inlineLet (Let [] e) = inlineLet e
inlineLet (Let [(v, rhs)] e)
  | v `elem` freeVars rhs = inlineLet (subst mayTerminate v e)
  | otherwise = inlineLet (subst rhs v e)
inlineLet (Let bs e) = inlineLet (substMany e s)
  where s = [(mayTerminate, v) | (v, e) <- bs]
inlineLet e = descend inlineLet e

splitLet :: Exp -> Exp
splitLet (Let bs e) = foldr Let (inlineLet e) (letGroups bs')
  where bs' = [(v, inlineLet e) | (v, e) <- bs]
splitLet e = descend splitLet e

-- Unroll a recursive group by one level
unroll :: [Decl] -> [Decl]
unroll ds = map (\d -> d { funcRhs = unrollExp ds (funcRhs d) } ) ds

unrollExp :: [Decl] -> Exp -> Exp
unrollExp ds (Fun f) =
  case lookupFuncs f ds of
    Func f [] rhs:_ -> rhs
    Func f es rhs:_ -> mayTerminate
    _ -> Fun f
unrollExp ds (App (Fun f) es) =
    case lookupFuncs f ds of
      Func f args rhs:_
        | length args <= length es ->
            let vs = map (\(Var v) -> v) args
                (es0, es1) = splitAt (length vs) es'
                rhs' = substMany rhs (zip es0 vs)
            in  mkApp rhs' es1
        | otherwise -> mayTerminate
      _ -> mkApp (Fun f) es'
  where es' = map (unrollExp ds) es
unrollExp ds e = descend (unrollExp ds) e

mkApp :: Exp -> [Exp] -> Exp
mkApp e [] = e
mkApp e es = App e es

-- Replace recursive calls with bottom
bottomise :: [Decl] -> [Decl]
bottomise ds = map (\d -> d { funcRhs = bottomiseExp ds (funcRhs d) } ) ds

bottomiseExp :: [Decl] -> Exp -> Exp
bottomiseExp ds (Fun f) =
  case lookupFuncs f ds of
    Func f es rhs:_ -> bottom
    _ -> Fun f
bottomiseExp ds (App (Fun f) es) =
    case lookupFuncs f ds of
      Func f args rhs:_ -> bottom
      _ -> mkApp (Fun f) es'
 where es' = map (bottomiseExp ds) es
bottomiseExp ds e = descend (bottomiseExp ds) e

-- Check for a fixed point
isFixedPoint :: Strictness -> Decl -> Decl -> Bool
isFixedPoint p d0 d1 = fp p args d0 d1
  where
    arity = length (funcArgs d0)
    args = map (Var . show) [0..arity-1]

fp :: Strictness -> [Exp] -> Decl -> Decl -> Bool
fp p xs d0 d1 =
  case eval p (App (Fun "=") [lhs, rhs]) of
    Int 0 -> False
    Int 1 -> True
    Var v -> and [fp p ys d0 d1 | ys <- refine xs (read v :: Int)]
  where
    args0 = map (\(Var v) -> v) (funcArgs d0)
    args1 = map (\(Var v) -> v) (funcArgs d1)
    lhs = substMany (funcRhs d0) (zip xs args0)
    rhs = substMany (funcRhs d1) (zip xs args1)
    
refine :: [Exp] -> Int -> [[Exp]]
refine xs i = [replace bottom, replace mayTerminate]
  where replace x = [if i == j then x else y | (y, j) <- zip xs [0..]]

-- Search for a fixed point of a recursive group
findFixedPoint :: Strictness -> [Decl] -> [Decl]
findFixedPoint p ds
  | and (zipWith (isFixedPoint p) ds0 ds1) = ds0
  | otherwise = findFixedPoint p ds'
  where
    ds0 = bottomise ds
    ds' = unroll ds
    ds1 = bottomise ds'

-- Determine strictness of a function in each arg
funcStrictness :: Strictness -> Decl -> [Bool]
funcStrictness p d = map (str p) es
  where
    arity = length (funcArgs d)
    vs = map (\(Var v) -> v) (funcArgs d)
    es = [substMany (funcRhs d) (zip args vs) | args <- oneHots arity]

str :: Strictness -> Exp -> Bool
str p e = case eval p e of { Int 0 -> True ; _ -> False }

oneHots :: Int -> [[Exp]]
oneHots n = map hot [0..n-1]
  where hot i = replicate i mayTerminate
             ++ [bottom]
             ++ replicate (n-1-i) mayTerminate

-- Perform strictness analysis on recursive groups
analyse :: [[Decl]] -> Strictness
analyse = anal []

anal p [] = p
anal p (ds:rest) = anal (ss ++ p) rest
  where
    fp = findFixedPoint p ds
    ss = [(funcName f, funcStrictness p f) | f <- fp]

-- Top-level strictness analyser
-- (Assumes that pattern matching has been desugared)
strictnessAnalysis :: Prog -> Strictness
strictnessAnalysis =
    analyse
  . callGroups
  . onExp inlineLet
  . onExp splitLet
  . onExp abstr
