module Flite.Strictify
  ( strictifyPrim
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

-- Arrange primitive applications according to Memo 40
strictifyPrim :: Prog -> Prog
strictifyPrim = onExp prim
  where
    prim (App (Fun f) (a:b:rest))
      | isUnaryPrim f = mkApp result (map prim rest)
      where result = App (prim a) [Fun f, prim b]
    prim (App (Fun f) (a:b:rest))
      | isPrimId f = mkApp result (map prim rest)
      where (a', b') = (prim a, prim b)
            result = if isInt a' && not (isInt b')
                       then App b' [Fun ("swap:"++f), a']
                       else App a' [Fun f, b']
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
