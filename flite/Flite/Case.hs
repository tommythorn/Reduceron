module Flite.Case (caseElim, caseElimWithCaseStack) where

import Flite.Syntax
import Flite.Traversals
import Flite.Descend
import Flite.State
import Control.Monad
import Data.List as List
import Data.Set as Set
import Data.Map as Map

-- Assumes that pattern matching has been desugared.

caseElim :: Prog -> Prog
caseElim = caseElim' False

caseElimWithCaseStack :: Prog -> Prog
caseElimWithCaseStack = caseElim' True

caseElim' :: Bool -> Prog -> Prog
caseElim' cstk p = elim cstk fs (expandCase ft p)
  where
    fs = families p
    ft = familyTable fs

type Family = Set (Id, Int)

families :: Prog -> [Family]
families p
  | check = fams
  | otherwise = error "A constructor cannot have different arities!"
  where
    check = let ids = [id | (id, _) <- Set.toList (Set.unions fams)]
            in  length ids == length (nub ids)

    fams = fixMerge (List.map Set.fromList ctrs)

    merge [] = []
    merge (f:fs) = Set.unions (f:same) : merge different
      where (same, different) = List.partition (overlap f) fs

    fixMerge fs = if length fs == length fs' then fs' else fixMerge fs'
      where fs' = merge fs

    overlap f0 f1 = not (Set.null (Set.intersection f0 f1))

    ctrs = fromExp fam p

    fam e = List.map (concatMap getCtr) (caseAlts e)

    getCtr (App (Con c) ps, e) = [(c, length ps)]
    getCtr (p, e) = []

familyTable :: [Family] -> Map Id Family
familyTable fams =
  Map.fromList [(id, fam) | fam <- fams, (id, arity) <- Set.toList fam]

expandCase :: Map Id Family -> Prog -> Prog
expandCase table p = onExp expand p
  where
    expand (Case e ((Var v, rhs):as)) = expand (Let [(v, e)] rhs)
    expand (Case e alts@((App (Con c) ps, rhs):as)) = Case (expand e) alts'
      where alts' = [getAlt f n | (f, n) <- Set.toAscList (table Map.! c)]
            getAlt f n = head ([ (App (Con c) args, expand rhs)
                               | (App (Con c) args, rhs) <- alts
                               , c == f ] ++ [bottom f n])
            bottom f n = (App (Con f) (replicate n (Var "_")), Bottom)
    expand e = descend expand e

elim :: Bool -> [Family] -> Prog -> Prog
elim cstk fams p = concatMap comp p
  where
    ctrInfo = [ (f, (arity, i))
              | fs <- List.map Set.toAscList fams
              , ((f, arity), i) <- zip fs [0..] ]

    comp d =
      let ((_, ds), e) = runState (compFun (funcName d) (funcRhs d)) (1, [])
      in  (d { funcRhs = e } : ds)

    compFun fun (Con c)
      | Prelude.null cinfo = return Bottom
      | otherwise = return (Ctr c (fst $ head cinfo) (snd $ head cinfo))
      where cinfo = [ci | (d, ci) <- ctrInfo, c == d]
    compFun fun (Case e as) =
      return App `ap` compFun fun e `ap` calts fun as
    compFun fun e = descendM (compFun fun) e

    calts fun as = 
      do es' <- mapM (compFun fun) es
         let fvs = nub $ concat $ zipWith (freeVarsExcept) vss es'
         fs <- zipWithM (calt fun fvs) vss es'
         let alts = Alts fs (length fvs)
         return ([alts] ++ [Int 0 | cstk && List.null fvs] ++ List.map Var fvs)
      where (ps, es) = unzip as
            vss = List.map (\(App _ args) -> [v | Var v <- args]) ps

    calt fun fvs vs e =
      do n <- newAlt
         let name = fun ++ "#" ++ show n
         let args = vs ++ ["$ct" | not cstk || (cstk && List.null fvs)] ++ fvs
         addDecl (Func name (List.map Var args) e)
         return name

    newAlt = S (\(i, ds) -> ((i+1, ds), i))

    addDecl d = S (\(i, ds) -> ((i, ds ++ [d]), ()))
