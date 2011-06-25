module Compiler.Prog where

import Yhc.Core
import Data.List

type Prog = [(String, ([String], CoreExpr))]

toProg :: Core -> Prog
toProg = lift "main"
       . lift "Prelude.True"
       . lift "Prelude.False"
       . map f
       . filter isCoreFunc
       . coreFuncs
  where
    f (CoreFunc n as e)  =  (n, (as, e))

lift f p = a:bs
  where
    ([a], bs) = partition ((== f) . fst) p

isPrim "ADD_W" = True
isPrim "SUB_W" = True
isPrim "EQ_W"  = True
isPrim "NE_W"  = True
isPrim "LE_W"  = True
isPrim _       = False
