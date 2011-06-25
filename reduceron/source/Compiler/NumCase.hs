module Compiler.NumCase where

import Yhc.Core

elimNumCase                 :: Core -> Core
elimNumCase core            =  applyBodyCore ncExp $
                                 core { coreDatas = coreDatas core ++ x }
  where
    x                       = 
      case coreCtorDataMaybe core trueCtr of
        Nothing -> let cs = [CoreCtor "Prelude.True" [],
                             CoreCtor "Prelude.False" []]
                   in [CoreData "Prelude.Bool" [] cs]
        _       -> []

ncExp                       :: CoreExpr -> CoreExpr
ncExp (CoreCase e as)
  | any (isPrimLit.fst) as  =  foldr (toIfte e') (snd (last as')) (init as')
  | otherwise               =  CoreCase e' as'
  where 
    e'                      =  ncExp e
    as'                     =  map (\(p, e) -> (p, ncExp e)) as
ncExp (CoreApp f es)        =  CoreApp (ncExp f) (map ncExp es)
ncExp (CoreLet ds e)        =  CoreLet ds' e'
  where
    ds'                     =  map (\(v, e) -> (v, ncExp e)) ds
    e'                      =  ncExp e
ncExp (CoreLam vs e)        =  CoreLam vs (ncExp e)
ncExp (CorePos _ e)         =  ncExp e
ncExp e                     =  e

toIfte e (p,rhs) els        =  CoreCase (equal e p) [
                                 (CoreApp (CoreCon trueCtr) [], rhs)
                               , (CoreApp (CoreCon falseCtr) [], els) ]
  where
    equal e0 e1             =  CoreApp (CoreFun "EQ_W") [e0, e1]

isPrimLit (CoreInt _)       =  True
isPrimLit (CoreInteger _)   =  error "Integer not supported"
isPrimLit (CoreChr _)       =  error "Char not supported"
isPrimLit (CoreFloat _)     =  error "Float not supported"
isPrimLit (CoreDouble _)    =  error "Double not supported"
isPrimLit _                 =  False

trueCtr                     =  "Prelude.True"
falseCtr                    =  "Prelude.False"
