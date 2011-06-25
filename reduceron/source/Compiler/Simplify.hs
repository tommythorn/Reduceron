module Compiler.Simplify where

import Yhc.Core
import Data.Generics.Play
import Compiler.Prog

simp :: Prog -> Prog
simp p = map (\(n, (as, e)) -> (n, (as, assoc e))) p

assoc :: CoreExpr -> CoreExpr
assoc = traverse f
  where
    f (CoreApp (CoreApp a xs) ys) = CoreApp a (xs ++ ys)
    f e = e
