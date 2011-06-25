module Compiler.Prepare where

import Yhc.Core
import Compiler.Church
import Compiler.LambdaLift
import Compiler.NumCase
import Compiler.ElimPrim
import Compiler.Prog
import Compiler.Simplify

prepare  :: Core -> Prog
prepare  =  simp
         .  elimPrim
         .  toProg
         .  coreLambdaLift
         .  church
         .  elimNumCase
         .  removeRecursiveLet
         .  uniqueBoundVarsCore
