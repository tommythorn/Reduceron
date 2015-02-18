module GHC-Flite.Translate where

import qualified Flite.Syntax as Flite

import CoreSyn (Alt, AltCon (..), Bind (..), CoreProgram, CoreBind, Expr (..))
import Var (Var, Id, idDetails, varName)
import Unique (getUnique)
import Literal (Literal (LitInteger))
import Unique (Uniquable)
import TypeRep (Type (..))
import IdInfo (IdDetails (PrimOpId, ClassOpId))
import PrimOp (PrimOp (IntAddOp, WordAddOp))
import Class (classKey)

translate :: CoreProgram -> Flite.Prog
translate = map translateBind

translateBind :: CoreBind -> Flite.Decl
translateBind (NonRec var expr) = Flite.Func { Flite.funcName = uniqueName $ var
                                             , Flite.funcArgs = []
                                             , Flite.funcRhs  = translateExpr expr }
translateBind (Rec exprs)     = undefined

translateExpr :: Expr Var -> Flite.Exp
translateExpr (Var id) = case idDetails id of
                            (PrimOpId primOp) -> Flite.Fun . translatePrimOp $ primOp
                            (ClassOpId klass) -> Flite.Fun $ "class" ++ (show . classKey $ klass)
                            _                 -> Flite.Var . uniqueName $ id

translateExpr (Lit lit) = translateLit lit

-- Ignore application if arg is a Type
translateExpr (App func (Type _))  = translateExpr func
translateExpr (App func arg)  = Flite.App (translateExpr func) [(translateExpr arg)]

translateExpr (Lam id body)   = Flite.Lam [uniqueName id] $ translateExpr body

translateExpr (Let bind body) = let decl = translateBind bind
                                in Flite.Let [(Flite.funcName decl, Flite.funcRhs decl)] $ translateExpr body

translateExpr (Case expr id ty alts) = Flite.Case (translateExpr expr) (map translateAlt alts)

translateExpr (Cast expr _coercion) = Flite.Var "unknown cast"
translateExpr (Tick _id _expr)      = Flite.Var "unknown tick"
translateExpr (Type ty)             = translateType ty
translateExpr (Coercion _coercion)  = Flite.Var "unknown coercion"

translatePrimOp :: PrimOp -> Flite.Id
translatePrimOp (WordAddOp) = "(+)"
translatePrimOp _          = "(?)"

translateLit :: Literal -> Flite.Exp
translateLit (LitInteger n _) = Flite.Int . fromInteger $ n
translateLit _ = Flite.Var "unknown literal"

translateAlt :: Alt Var -> Flite.Alt
translateAlt (DataAlt dataCon, vars, expr) = (Flite.Con $ uniqueName dataCon, translateExpr expr)
translateAlt (LitAlt _literal, _vars, _expr)  = error "LitAlt encountered in translateAlt"

translateType :: Type -> Flite.Exp
translateType (TyVarTy var) = Flite.Var $ "TyVarTy " ++ uniqueName var
translateType (AppTy ty1 ty2) = Flite.Var $ "AppTy " ++ "ty1" ++ " " ++ "ty2"
translateType (TyConApp tycon args) = Flite.Var $ "TyConApp"
translateType (FunTy ty1 ty2) = Flite.Var $ "FunTy"


uniqueName :: (Uniquable a) => a -> String
uniqueName = show . getUnique
