module SyntaxUtil(dropPatAs, infixFun, isCon
	, isExpInt, isExpIrr, isExpVar, isVar, isNK
	, isTypeVar
	) where

import Syntax
import SyntaxPos

-- CaseHelp

isVar (ExpVar _ _) = True
isVar (PatWildcard _) = True
isVar _ = False

isCon (ExpCon _ _) = True
isCon (ExpApplication _ (p:_)) = isCon p
isCon _ = False

isExpInt (ExpLit _ (LitInt _ _)) = True
isExpInt _ = False

isNK (PatNplusK _ _ _ _ _ _) = True
isNK _ = False

isExpIrr (PatIrrefutable _ _) = True
isExpIrr _ = False

dropPatAs (PatAs _ _ pat) = dropPatAs pat
dropPatAs pat = pat

isExpVar (ExpVar _ _) = True
isExpVar _ = False


-- Rename

infixFun es =
  case break isExpVarOp es of
    (e1,ExpVarOp pos fun:e2) ->
      Just (ExpInfixList (getPos e1) e1,pos,fun,ExpInfixList (getPos e2) e2)
    _ -> Nothing

isExpVarOp (ExpVarOp _ _) = True
isExpVarOp _ = False

isTypeVar (TypeVar _ _ ) = True
isTypeVar _ = False
