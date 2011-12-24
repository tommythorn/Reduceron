{-# OPTIONS -fno-warn-name-shadowing #-}
{- ---------------------------------------------------------------------------
Make parts of Haskell syntax tree
-}
module MkSyntax
	( mkAppExp, mkAppInst, mkCase, mkDeclClass
	, mkDeclFun, mkDeclPat, mkDeclPatFun, mkEnumFrom
	, mkEnumThenFrom, mkEnumToFrom, mkEnumToThenFrom
        , mkLambda, mkLet, mkDo, mkFieldExp,mkExpList
	, mkExpListComp, mkIf, mkInfixList
	, mkInstList, mkInt, mkParExp, mkParInst, mkParType
	, mkTypeList, mkPatNplusK, mkParLhs
  , mkSweetListEnum, mkSweetListComp
	) where

import Extra(Pos,noPos,mergePos,mergePoss)
import TokenId
import Syntax
import SyntaxPos(HasPos(getPos))


mkAppExp :: [Exp id] -> Exp id
mkCase :: Pos -> Exp id -> [Alt id] -> Exp id
mkDo :: Pos -> [Stmt id] -> Exp id
mkEnumFrom :: Pos -> Exp TokenId -> Exp TokenId
mkEnumThenFrom :: Pos -> Exp TokenId -> Exp TokenId -> Exp TokenId
mkEnumToFrom :: Pos -> Exp TokenId -> Exp TokenId -> Exp TokenId
mkEnumToThenFrom :: Pos
			-> Exp TokenId
			-> Exp TokenId
			-> Exp TokenId
			-> Exp TokenId
mkExpList :: Pos -> [Exp id] -> Pos -> Exp id
mkExpListComp :: Pos -> [Qual TokenId] -> Exp TokenId -> Exp TokenId
mkFieldExp :: Pos -> id -> Exp id -> Field id
mkIf :: Pos -> Exp id -> Exp id -> Exp id -> Exp id
mkInfixList :: [Exp id] -> Exp id
mkInstList :: Pos -> TokenId -> Type TokenId
mkInt :: Pos -> Int -> Exp id
mkLambda :: Pos -> [Pat id] -> Exp id -> Exp id
mkLet :: Pos -> Decls id -> Exp id -> Exp id
mkParExp :: Pos -> [Exp TokenId] -> Pos -> Exp TokenId
mkParInst :: Pos -> [(Pos, TokenId)] -> Type TokenId
mkParLhs :: Pos -> Exp id -> [Exp id] -> Exp id
mkPatNplusK :: (Pos, t) -> (Pos, Lit Boxed) -> Exp t
mkTypeList :: Pos -> Type TokenId -> Type TokenId
notOp :: Exp t -> Bool

mkParType :: Pos -> [Type TokenId] -> Type TokenId
mkParType _p [t] = t
mkParType p ts  = TypeCons p (t_Tuple (length ts)) ts


mkAppInst :: (Pos,a) -> [(Pos,a)] -> Type a
mkAppInst (p,c) ts = TypeCons p c (map (uncurry TypeVar) ts)


mkInfixList [e] = e
mkInfixList es = ExpInfixList (getPos es) es


mkParInst _p [_t] = error ("mkParInst on singleton list")
mkParInst p ts  = TypeCons p (t_Tuple (length ts)) (map (uncurry TypeVar) ts)


mkInstList p id = TypeCons p t_List [TypeVar p id]


mkDeclPat :: (Pos,a) -> Exp a -> Exp a -> Rhs a -> Decls a -> Decl a
mkDeclPat (pv,var) op e@(ExpInfixList pos _es) gdexps w =
	DeclPat (Alt (ExpInfixList pos [ExpVar pv var,op,e]) gdexps w)
mkDeclPat (pv,var) op e gdexps w =
	DeclPat (Alt (ExpInfixList pv [ExpVar pv var,op,e]) gdexps w)


mkDeclFun :: (Pos,a) -> [Pat a] -> Rhs a -> Decls a -> Decl a
--mkDeclFun (pv,var) [] gdexps w =
--	DeclPat (Alt (ExpVar pv var) gdexps w)
mkDeclFun (pv,var) pats gdexps w =
  DeclFun (mergePoss [pv `min` getPos pats,getPos gdexps,getPos w]) 
    var [Fun pats gdexps w]


mkDeclPatFun :: Alt a -> Decl a
mkDeclPatFun  (Alt (ExpVar pos fun) gdexps w) =
  DeclFun (mergePoss [pos,getPos gdexps,getPos w]) fun [Fun [] gdexps w]
--        DeclPat (Alt (ExpVar pos fun) gdexps w)
mkDeclPatFun  (Alt (ExpInfixList _ [ExpVar pos fun]) gdexps w) =
  DeclFun (mergePoss [pos,getPos gdexps,getPos w]) fun [Fun [] gdexps w]
mkDeclPatFun  (Alt (ExpInfixList _ (ExpVar pos fun:qop:args)) gdexps w) 
  | notOp qop = DeclFun (mergePoss [pos,getPos gdexps,getPos w]) fun 
                  [Fun (qop:args) gdexps w]
mkDeclPatFun alt = DeclPat alt

notOp (ExpConOp _ _) = False
notOp (ExpVarOp _ _) = False
notOp _ = True

--mkRevTypeArrow p a b = TypeCons p t_Arrow [b,a]
mkTypeList p t = TypeCons p t_List [t]

-- passes position of lambda
mkLambda pos pats e = 
  let p = mergePos pos (getPos e) in p `seq` ExpLambda p pats e

-- passes position of let
mkLet pos decls e =
  let p = mergePos pos (getPos e) in p `seq` ExpLet p decls e

-- passes position of do
mkDo pos stmts =
  let p = mergePos pos (getPos stmts) in p `seq` ExpDo p stmts

-- passes position of if
mkIf pos e1 e2 e3 =
  let p = mergePos pos (getPos e3) in p `seq` ExpIf p e1 e2 e3

-- passes position of case
mkCase pos e alts =
  let p = mergePos pos (getPos alts) in p `seq` ExpCase p e alts

-- list enumerations in sugary form
mkSweetListEnum ::
  Pos -> Exp TokenId -> Maybe (Exp TokenId) -> Maybe (Exp TokenId) -> Pos ->
  Exp TokenId
mkSweetListEnum posl eFrom meThen meTo posr =
  let p = mergePos posl posr in p `seq` ExpListEnum p eFrom meThen meTo

-- for de-sugared list comprehensions
mkEnumFrom pos eFrom =
        ExpApplication pos [ExpVar pos tenumFrom,eFrom]
mkEnumToFrom pos eTo eFrom =
        ExpApplication pos [ExpVar pos tenumFromTo,eFrom,eTo]
mkEnumThenFrom pos eThen eFrom =
        ExpApplication pos [ExpVar pos tenumFromThen,eFrom,eThen]
mkEnumToThenFrom pos eTo eThen eFrom =
        ExpApplication pos [ExpVar pos tenumFromThenTo,eFrom,eThen,eTo]


mkAppExp [] = error "mkAppExp"
mkAppExp [e] = e
mkAppExp es@[e1,e2] = 
  ExpApplication (mergePos (getPos e1) (getPos e2)) es
mkAppExp es@(e1:e2:es') =
  ExpApplication (mergePos (min (getPos e1) (getPos e2)) (getPos (last es'))) 
    es
  -- operator of infix expression is in front and first argument next

-- passes positions of left and right parenthesis
mkParExp posl es posr = p `seq` e
  where
  p = mergePos posl posr
  e = case es of
      [ExpConOp pos' id] -> ExpBrack p (ExpCon pos' id)
      [ExpVarOp pos' id] -> ExpBrack p (ExpVar pos' id)
      [e]                -> ExpBrack p e
      es                 -> ExpApplication p (ExpCon p (t_Tuple (length es)):es)

{-
-- Previous definition ignored posl and posr in some cases but
-- did not need require an ExpBrack construction
mkParExp posl [ExpConOp pos' id] posr = ExpCon pos' id
mkParExp posl [ExpVarOp pos' id] posr = ExpVar pos' id
mkParExp posl [e] posr = e
mkParExp posl es  posr = 
  let p = mergePos posl posr 
  in p `seq` ExpApplication p (ExpCon p (t_Tuple (length es)):es)
-}

mkFieldExp pos ident exp =
  let p = mergePos pos (getPos exp) in p `seq` FieldExp pos ident exp

-- passes positions of left and right brackets
mkExpList posl exps posr =
  let p = mergePos posl posr in p `seq` ExpList p exps

mkParLhs pos app args = ExpApplication pos (app:args)

-- combineGroups (DeclsParse d1) (DeclsParse d2) = DeclsParse (d1++d2)
-- 
-- mkDeclClass ctx (pos,cls) (_,arg) (csigns,valdefs) =
--             DeclClass pos ctx cls arg (combineGroups csigns valdefs)

-- changed in H98 to:
mkDeclClass ::
   [Context b] -> (Pos, b) -> [(a, b)] -> [FunDep b] -> Decls b -> Decl b
mkDeclClass ctx (pos,cls) posargs fundeps cdecls =
    DeclClass pos ctx cls (map snd posargs) fundeps cdecls


mkExp_Colon :: Pos -> Exp TokenId
mkExp_Colon pos  = ExpCon pos t_Colon

mkExp_filter :: Pos -> Exp TokenId
mkExp_filter pos  = ExpVar pos t_filter

mkExp_foldr :: Pos -> Exp TokenId
mkExp_foldr pos  = ExpVar pos t_foldr

mkExp_x :: Pos -> Exp TokenId
mkExp_x pos  = ExpVar pos t_x
mkExp_y :: Pos -> Exp TokenId
mkExp_y pos  = ExpVar pos t_y

-- list comprehensions in sugary form
mkSweetListComp ::
  Pos -> Exp TokenId -> [Qual TokenId] -> Pos -> Exp TokenId
mkSweetListComp posl e qs posr =
  let p = mergePos posl posr in p `seq` ExpListComp p e qs

-- de-sugared list comprehensions
mkExpListComp pos qs e = ExpApplication noPos [trans pos qs e,ExpList noPos []]
 where
  trans pos [] e =
    ExpApplication pos
        [mkExp_Colon pos
        ,e
        ]
  trans pos (QualLet decls:qs) e =
    ExpLet pos decls (trans pos qs e)
  trans pos (QualExp exp:qs) e =
    ExpApplication pos
        [mkExp_filter pos
        ,exp
        ,trans pos qs e
        ]
  trans pos (QualPatExp pat exp:qs) e =
    ExpApplication pos
        [mkExp_foldr pos
        ,ExpLambda pos
            [mkExp_x noPos,mkExp_y pos]
            (ExpCase noPos (mkExp_x pos)
              [Alt pat
                   (Unguarded (ExpApplication pos [trans pos qs e,mkExp_y pos]))
                   (DeclsParse [])
              ,Alt (PatWildcard pos)
                   (Unguarded (mkExp_y pos))
                   (DeclsParse [])
              ]
            )
        ,exp
        ]


mkInt pos i = ExpLit pos (LitInt Boxed i)

mkPatNplusK (pos,tid) (posi,integer) =
  let p = mergePos pos posi in p `seq` 
    PatNplusK p tid undefined (ExpLit posi integer) undefined undefined
-- While parsing (n+k), can't choose a unique replacement identifier n',
-- so leave some fields to be filled in later.

--  let k = ExpLit posi integer in
--  PatNplusK pos tid undefined k (ExpApplication pos [t_lessequal,k])
--                                (ExpApplication pos [t_subtract,k])

{- End Module MkSyntax ------------------------------------------------------}
