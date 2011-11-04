module Flite.IntInfer
  ( Intness       -- type Intness = [(Id, [Bool])]
  , intInfer      -- :: Prog -> Intness
  ) where

import Flite.Syntax
import Flite.Traversals
import Flite.Dependency

-- Defines for each function which arguments are primitive integers
type Intness = [(Id, [Bool])]

lookupIntness :: Intness -> Id -> [Bool]
lookupIntness p f = case lookup f p of { Nothing -> [] ; Just bs -> bs }

-- Is a given variable an integer in a given expression?
isInt :: Intness -> Id -> Exp -> Bool
isInt p v (App (Fun f) es)
  | isBinaryPrim f = anyInt p [True, True] v es
  | isUnaryPrim f = anyInt p [True] v es
  | otherwise = anyInt p mask v es
  where mask = lookupIntness p v
isInt p v (App e es) = any (isInt p v) (e:es)
isInt p v (Let bs e)
  | any ((== v) . fst) bs = False
  | otherwise = any (isInt p v) (e:map snd bs)
isInt p v (Case e alts) =
  isInt p v e || or [isInt p v e | (pat, e) <- alts, v `notElem` patVars pat]
isInt p v e = False

anyInt :: Intness -> [Bool] -> Id -> [Exp] -> Bool
anyInt p mask v es = any (isVar v) es' || any (isInt p v) es
  where es' = [e | (b, e) <- zip mask es, b]

isVar :: Id -> Exp -> Bool
isVar v (Var w) = v == w
isVar v e = False

-- Perform integer inference on recursive groups
infer :: [[Decl]] -> Intness
infer = inf []

inf p [] = p
inf p (ds:rest) = inf (is ++ p) rest
  where is = funcIntness p ds

funcIntness :: Intness -> [Decl] -> Intness
funcIntness p ds = fi initial ds
  where
    initial = [(funcName d, map (const False) (funcArgs d)) | d <- ds]
    fi g ds = if g == g' then g else fi g' ds
      where g' = [(funcName d, intness (g ++ p) d) | d <- ds]

intness :: Intness -> Decl -> [Bool]
intness p d = map (\(Var v) -> isInt p v (funcRhs d)) (funcArgs d)

-- Computes for each function which arguments are primitive integers
-- (The analysis is conservative)
intInfer :: Prog -> Intness
intInfer = infer . callGroups
