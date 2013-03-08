{-# OPTIONS -fno-warn-incomplete-patterns #-}
module SyntaxPos(Pos,HasPos(..)) where

import Extra(Pos,noPos,mergePos,mergePoss)
import Syntax

class HasPos a where
        getPos :: a -> Pos

instance HasPos (Decls a) where
     getPos (DeclsParse decls) = getPos decls
     getPos (DeclsScc decls) = getPos decls

instance HasPos (DeclsDepend a) where
     getPos (DeclsNoRec decl) = getPos decl
     getPos (DeclsRec   decls) = getPos decls

instance HasPos (Decl a) where
  getPos (DeclType simple ty) = mergePos (getPos simple) (getPos ty)
  getPos (DeclDataPrim pos _ _) = pos
  getPos (DeclData _ ctx simple constrs derives) =
    mergePoss [getPos ctx,getPos simple,getPos constrs,getPosList derives]
  getPos (DeclConstrs pos _ _) = pos
  getPos (DeclClass pos _ _ _ _ _) = pos
  getPos (DeclInstance pos _ _ _ _) = pos
  getPos (DeclDefault tys) = getPos tys
  getPos (DeclVarsType ((pos,_):_) _ ty) = mergePos pos (getPos ty)
  getPos (DeclFun pos _fun _funs)       = pos
  getPos (DeclPrimitive pos _fun _a _t)  = pos
  getPos (DeclForeignImp pos _ _s _fun _a _c _t _) = pos
  getPos (DeclForeignExp pos _ _s _fun _t) = pos
  getPos (DeclPat alt)                = getPos alt
  getPos (DeclIgnore _str)             = noPos
  getPos (DeclError _str)              = noPos

instance HasPos (Entity a) where
    getPos (EntityVar        pos _)   = pos
    getPos (EntityConClsAll  pos _)   = pos
    getPos (EntityConClsSome pos _ _) = pos

instance HasPos (Alt a) where
    getPos (Alt pat rhs locals) =
      mergePoss [getPos pat,getPos rhs,getPos locals]

instance HasPos (Fun a) where
    getPos (Fun pats rhs locals) =
      mergePoss [getPos pats,getPos rhs,getPos locals]

instance HasPos (Rhs a) where
    getPos (Unguarded e) = getPos e
    getPos (Guarded gdes) =
      mergePos (getPos (fst (head gdes))) (getPos (snd (last gdes)))

instance HasPos (Stmt a) where
  getPos (StmtExp stExp) = getPos stExp
  getPos (StmtBind pat stExp) = mergePos (getPos pat) (getPos stExp)
  getPos (StmtLet decls) = getPos decls

instance HasPos (Exp a) where
  getPos (ExpDict        stExp)       = getPos stExp
  getPos (ExpScc         _str stExp)   = getPos stExp
  getPos (ExpLambda      pos _ _)   = pos
  getPos (ExpLet         pos _ _)   = pos
  getPos (ExpDo          pos _)     = pos
  getPos (ExpCase        pos _ _)   = pos
  getPos (ExpFail)                  = error "No position for ExpFail"
  getPos (ExpIf          pos _ _ _) = pos
  getPos (ExpType        pos _ _ _) = pos
  getPos (ExpRecord      stExp fdefs) = mergePos (getPos stExp) (getPos fdefs)
  getPos (ExpApplication pos _ )    = pos
  getPos (ExpInfixList   pos _)     = pos
  getPos (ExpVar         pos _)     = pos
  getPos (ExpCon         pos _)     = pos
  getPos (ExpVarOp       pos _)     = pos
  getPos (ExpConOp       pos _)     = pos
  getPos (ExpLit         pos _)     = pos
  getPos (ExpList        pos _)     = pos
  getPos (ExpListComp    pos _ _)   = pos
  getPos (ExpListEnum    pos _ _ _) = pos
  getPos (ExpBrack       pos _)     = pos
  getPos (Exp2           pos _i1 _i2) = pos
  getPos (PatAs          pos _ _)   = pos
  getPos (PatWildcard    pos)       = pos
  getPos (PatIrrefutable pos _)     = pos
  getPos (PatNplusK      pos _ _ _ _ _) = pos


instance HasPos a => HasPos [a] where
    -- assumes that first and last element have proper positions
    getPos [] = noPos
    getPos xs = mergePos (getPos (head xs)) (getPos (last xs))

instance (HasPos a,HasPos b) => HasPos (a,b) where  -- used on GdExp
    getPos (a,b) = mergePos (getPos a) (getPos b)

instance HasPos (Simple a) where
    getPos (Simple pos _ _) = pos

instance HasPos (Type a) where
    getPos (TypeApp  t1 t2) = mergePos (getPos t1) (getPos t2)
    -- pos is position of constructor, not whole type, which shall be returned
    getPos (TypeCons pos _ (t:ts)) = mergePos (min pos (getPos t)) (getPos ts)
    getPos (TypeCons pos _ ts) = mergePos pos (getPos ts)
    getPos (TypeVar   pos _)   = pos
    getPos (TypeStrict  pos _)   = pos

instance HasPos (Context a) where
    getPos (Context pos _ _) = pos

instance HasPos (FixId a) where
    getPos (FixCon pos _a) = pos
    getPos (FixVar pos _a) = pos

instance HasPos (Field a) where
    getPos (FieldExp pos _ _) = pos
    getPos (FieldPun pos _) = pos

instance HasPos (Constr a) where
    getPos (Constr pos _ _) = pos
    getPos (ConstrCtx _ _ pos _ _) = pos


getPosList :: [(Pos,a)] -> Pos
getPosList [] = noPos
getPosList xs = mergePos (fst (head xs)) (fst (last xs))
