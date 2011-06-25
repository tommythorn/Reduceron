-- This module belongs in Yhc.Core; will be moved after it becomes stable
-- Neil Mitchell <ndm@cs.york.ac.uk>

module Compiler.LambdaLift(coreLambdaLift) where

import Yhc.Core.Show
import Yhc.Core.Type
import Yhc.Core.Play
import Control.Monad.State
import Yhc.Core.FreeVar
import Data.List

coreLambdaLift :: Core -> Core
coreLambdaLift = coreLambdaName . coreLambdaClosure


coreLambdaName :: Core -> Core
coreLambdaName core = core{coreFuncs = concatMap f $ coreFuncs core}
    where
        f :: CoreFunc -> [CoreFunc]
        f x = let (a,b) = runState (mapUnderCoreM (g (coreFuncName x)) x) (0,[]) in a : snd b

        g name (CoreLam bind body) = do
            (i,rest) <- get
            let newname = name ++ "__yhc__lam" ++ show i
            put (i+1, CoreFunc newname bind body : rest)
            return $ CoreFun newname
        g name x = return x


coreLambdaClosure :: Core -> Core
coreLambdaClosure = mapUnderCore f
    where
        f x@(CoreLam bind body) = coreApp (CoreLam (free++bind) body) (map CoreVar free)
            where free = nub $ collectFreeVars x
        f x = x
