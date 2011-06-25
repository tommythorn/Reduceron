module Compiler.ElimPrim where

import Yhc.Core
import Data.Generics.Play
import Compiler.Prog

elimPrim :: Prog -> Prog
elimPrim =  map (\(f, (vs, e)) -> (f, (vs, elimPrim' e)))

elimPrim' :: CoreExpr -> CoreExpr
elimPrim' = traverse f
  where
    f (CoreApp (CoreFun fun) [a, b])
      | fun == "SEQ" = b
      | fun == "Prelude.Prelude.Num.Prelude.Int.-" =
          elimPrim' (CoreApp (CoreFun "SUB_W") [a, b])
      | fun == "Prelude.Prelude.Num.Prelude.Int.+" =
          elimPrim' (CoreApp (CoreFun "ADD_W") [a, b])
      | fun == "Prelude.Prelude.Num.Prelude.Int./=" =
          elimPrim' (CoreApp (CoreFun "NE_W") [a, b])
      | isPrim fun   =
          case (isCoreLit a, isCoreLit b) of
            (False, False) -> CoreApp b [CoreApp a [CoreFun fun]]
            (False, True)  -> CoreApp a [CoreFun fun, b]
            (True, False)  -> CoreApp b [CoreApp (CoreFun fun) [a]]
            (True, True)   -> CoreApp (CoreFun fun) [a, b]
      | otherwise    = CoreApp (CoreFun fun) [a, b]
    f e = e

isCoreLit (CoreInt _) = True
isCoreLit _ = False
