--------------------------------------------------------------
-- The core of the translation from a Haskell subset to F-lite
-- Colin Runciman, May 2010
--------------------------------------------------------------

module Translate(translate, declTrans, infixPatsToFuns, mergeDeclFuns) where

import Prelude hiding (enumFrom,enumFromTo,enumFromThen,enumFromThenTo)
import Syntax
import Extra(Pos,noPos,mergePos,fromPos, mergePoss,chopBy)
import SyntaxPos
import PrettySyntax(ppType,ppExp,prettyPrintTokenId)
import Flags
import TokenId(TokenId,t_undef,
               mkUnqualifiedTokenId)
import List(groupBy,intersperse,span,last,transpose)

type Trans a = a -> a

multiItem :: [a] -> Bool
multiItem (_x:_y:_etc) = True
multiItem _         = False

infixPatsToFuns :: [Decl TokenId] -> [Decl TokenId]
infixPatsToFuns [] = []
infixPatsToFuns (d:ds) =
  d' : infixPatsToFuns ds
  where
  d' = case d of
       DeclPat (Alt (ExpInfixList _pos es) rhs decls)
         | any isVarOp es ->
           DeclFun (getPos d) id0 [Fun argPats rhs decls]
           where
           argPats = [argFrom pos leftExps, argFrom pos rightExps]
           (leftExps, ExpVarOp pos id0 : rightExps) =
             break isVarOp es        
       _other -> d
  isVarOp (ExpVarOp _ _) = True
  isVarOp _              = False

argFrom :: Pos -> [Exp id] -> Exp id
argFrom _ [e] = e
argFrom _ es  = ExpInfixList (mergePoss (map getPos es)) es

mergeDeclFuns :: [Decl TokenId] -> [Decl TokenId]
mergeDeclFuns [] = []
mergeDeclFuns [d] = [d]
mergeDeclFuns (d0:d1:ds) =
  case (d0, d1) of
  (DeclFun pos0 id0 funs0, DeclFun pos1 id1 funs1) ->
    if show id0 == show id1 then
      mergeDeclFuns
        (DeclFun (mergePos pos0 pos1) id0 (funs0 ++ funs1) : ds)
    else
      d0 : mergeDeclFuns (d1:ds)
  _other ->
    d0 : mergeDeclFuns (d1:ds)

-- IDEA: Lift to IO Monad and use Control.Exception to catch
-- translation failures or discards.  Then output can be
-- flite equivalents of translatable decls only.
-- Options for silent, commented or stderr response to failures.
translate :: Flags -> Module TokenId -> Module TokenId
translate flags (Module pos modid exps imps fixs decls) =
  Module pos modid exps imps fixs decls'
  where
  decls' = declsTrans True decls
 
declsTrans :: Bool -> Trans (Decls TokenId)
declsTrans top (DeclsParse ds) =
  DeclsParse ds'
  where
  ds' = map (declTrans top)
            (mergeDeclFuns (infixPatsToFuns ds))

declTrans :: Bool -> Decl TokenId -> Decl TokenId
declTrans top (DeclFun pos id0 funs) =
  if not top then
    case funs of
    [Fun [] (Unguarded e) (DeclsParse [])] -> df
    other -> cannotTranslate pos "complex local definition"
  else if not (uniformDeclFun df) then
    cannotTranslate pos "non-uniform pattern matching"
  else
    df
  where
  df = DeclFun pos id0 funs'
  funs' = map (funTrans (multiItem funs)) funs
declTrans _ (DeclPat a) =
  cannotTranslate (getPos $ DeclPat a) "pattern-binding declaration"
declTrans _ other =
  error "declTrans: unexpected kind of declaration" 

uniformDeclFun :: Decl TokenId -> Bool
uniformDeclFun (DeclFun pos id funs) =
  uniformPatLists $ transpose [ps | Fun ps _ _ <- funs]

uniformPatLists :: [[Exp TokenId]] -> Bool
uniformPatLists []       = True
uniformPatLists (ps:pss) =
  if any nonVar ps then
    all nonVar ps && all uniformPatLists (splitBy1stCons (ps:pss))
  else
    uniformPatLists pss

splitBy1stCons  :: [[Exp TokenId]] -> [[[Exp TokenId]]]
splitBy1stCons =
  map lift1stConsArgs . groupBy same1stCons . transpose
  where
  lift1stConsArgs pss = transpose [args p ++ ps | p:ps <- pss]

nonVar :: Exp TokenId -> Bool
nonVar (ExpVar _ _) = False
nonVar _            = True

same1stCons :: [Exp TokenId] -> [Exp TokenId] -> Bool
same1stCons (p1:_) (p2:_) = sameCons p1 p2

sameCons :: Exp TokenId -> Exp TokenId -> Bool
sameCons (ExpCon _ id1)
         (ExpCon _ id2) =
  show id1 == show id2
sameCons (ExpApplication _ (ExpCon _ id1 : _))
         (ExpApplication _ (ExpCon _ id2 : _)) =
  show id1 == show id2
sameCons _ _ = False

args :: Exp TokenId -> [Exp TokenId]
args (ExpCon _ _) = []
args (ExpApplication _ (_:ps))= ps
                             
funTrans :: Bool -> Trans (Fun TokenId)
funTrans oneOfMany (Fun ps rhs decls) =
  case decls of
  DeclsParse [] -> Fun ps' rhs' decls'
  other         -> Fun ps' (Unguarded (ExpLet noPos decls' e)) (DeclsParse [])
  where
  Unguarded e = rhs'
  ps'    = map patTrans ps
  rhs'   = rhsTrans oneOfMany rhs
  decls' = declsTrans False decls

altTrans :: Bool -> Trans (Alt TokenId)
altTrans oneOfMany (Alt p rhs decls) =
  case decls of
  DeclsParse [] -> Alt p' rhs' decls'
  other         -> Alt p' (Unguarded (ExpLet noPos decls' e)) (DeclsParse [])
  where
  Unguarded e = rhs'
  p'     = patTrans p
  rhs'   = rhsTrans oneOfMany rhs
  decls' = declsTrans False decls

rhsTrans :: Bool -> Trans (Rhs TokenId)
rhsTrans oneOfMany (Unguarded e) =
  Unguarded e'
  where
  e' = expTrans e
rhsTrans oneOfMany (Guarded ges) =
  cannotTranslate (getPos $ head ges) "guarded equation"

expTrans :: Trans (Exp TokenId)
expTrans (ExpLambda pos ps e) =
  cannotTranslate pos "lambda expression"
expTrans (ExpLet pos decls e) =
  ExpLet pos decls' e'
  where
  decls' = declsTrans False decls
  e' = expTrans e
expTrans (ExpDo pos stmts) =
  cannotTranslate pos "do expression"
expTrans (ExpCase pos e alts) =
  ExpCase pos e' alts'
  where
  e' = expTrans e
  alts' = map (altTrans (multiItem alts)) alts
expTrans (ExpIf pos ec et ef) =
  condTrans ec' et' ef'
  where
  ec' = expTrans ec
  et' = expTrans et
  ef' = expTrans ef
expTrans (ExpRecord e fields) =
  cannotTranslate (getPos e) "record expression"
expTrans (ExpApplication pos (e:es)) =
  ExpApplication pos (e':es')
  where
  e'  = expTrans e
  es' = map expTrans es
expTrans (ExpVar pos tid) =
  ExpVar pos tid
expTrans (ExpCon pos tid) =
  -- TEMPORARY FIX
  case show tid of
  "Prelude.[]" -> nil
  "()"         -> unit
  "Prelude.2"  -> pair
  "Prelude.3"  -> triple
  other        -> ExpCon pos tid
expTrans (ExpInfixList pos es) =
  expInfixListTrans pos es'
  where
  es' = map expTrans es
expTrans (ExpVarOp pos tid) =
  ExpVarOp pos tid
expTrans (ExpConOp pos tid) =
  ExpConOp pos tid
expTrans (ExpLit pos lit) =
  case lit of 
  LitInteger  _ _ -> yes
  LitRational _ _ -> no "rational"
  LitString   _ _ -> yes
  LitInt      _ _ -> yes
  LitDouble   _ _ -> no "double"
  LitFloat    _ _ -> no "float"
  LitChar     _ _ -> yes
  where
  yes  = ExpLit pos lit
  no s = cannotTranslate pos s
expTrans (ExpList pos es) =
  foldr cons' nil es'
  where
  cons' x xs = ExpApplication noPos [cons, x, xs]
  es' = map expTrans es
expTrans (ExpListComp pos e qs) =
  {-
  case qs of
  []                   -> ExpApplication noPos (expTrans e) nil
  QualExp b : qs'      -> condTrans (expTrans b) (ExpListComp noPos e qs') nil
  QualPatExp p e : qs' -> -- STUCK WITHOUT LAMBDA OR LOCAL FUN DEFN!
                          -- [e | p<-l, qs ] = let ok p = [e | qs]
                          -- 		                   ok _ = []
                          -- 		               in concatMap ok l
  QualLet decls : qs'  -> expTrans (ExpLet noPos decls (ExpListComp noPos e qs'))
  -}
  cannotTranslate pos "list comprehension"
{-
qualTrans n (QualPatExp p e) =
  (n', QualPatExp p e')
  where
  (n', e') = expTrans n e
qualTrans n (QualExp e) =
  (n', QualExp e')
  where
  (n', e') = ( binTickBox QualBinBox `after`
               expTrans ) n e
qualTrans n (QualLet decls) =
  (n', QualLet decls')
  where
  (n', decls') = declsTrans False n decls
-}
expTrans (ExpListEnum pos ef meth meto) =
  case (meth',meto') of
  (Nothing,Nothing) -> ExpApplication pos [enumFrom, ef']
  (Nothing,Just to) -> ExpApplication pos [enumFromTo, ef', to]
  (Just th,Nothing) -> ExpApplication pos [enumFromThen, ef', th]
  (Just th,Just to) -> ExpApplication pos [enumFromThenTo, ef', th, to]
  where
  ef'   = expTrans ef
  meth' = case meth of
          Nothing -> meth
          Just e  -> Just (expTrans e)
  meto' = case meto of
          Nothing -> meto
          Just e  -> Just (expTrans e)
expTrans (ExpBrack _pos e) =
  expTrans e
expTrans (ExpType pos e cxt ty) =
  cannotTranslate pos "type-annotated expression"
expTrans e =
  cannotTranslate (getPos e) "expression"

condTrans :: Exp TokenId -> Exp TokenId -> Exp TokenId -> Exp TokenId
condTrans e1 e2 e3 =
  ExpCase noPos e1 [alt true e2, alt false e3]
  where
  alt p e = Alt p (Unguarded e) (DeclsParse [])

true, false :: Exp TokenId
cons           = str2ConExp "Cons"
nil            = str2ConExp "Nil"
true           = str2ConExp "True"
false          = str2ConExp "False"
unit           = str2ConExp "Unit"
pair           = str2ConExp "Pair"
triple         = str2ConExp "Triple"
append         = str2VarExp "append"
enumFrom       = str2VarExp "enumFrom"
enumFromThen   = str2VarExp "enumFromThen"
enumFromTo     = str2VarExp "enumFromTo"
enumFromThenTo = str2VarExp "enumFromThenTo"
equal          = str2VarExp "=="
lesseq         = str2VarExp "<="
noteq          = str2VarExp "/="
plus           = str2VarExp "+"
minus          = str2VarExp "-"

str2ConExp :: String -> Exp TokenId
str2ConExp = ExpCon noPos . mkUnqualifiedTokenId

str2VarExp :: String -> Exp TokenId
str2VarExp = ExpVar noPos . mkUnqualifiedTokenId

-- the only operators translated are:
-- || infixr 2
-- && infixr 3
-- ==, /=, <, <=, >=, > infix 4
-- :, ++ infixr 5
-- +, - infixl 6
expInfixListTrans :: Pos -> [Exp TokenId] -> Exp TokenId
expInfixListTrans pos es =
  if isAnOp (head es) || isAnOp (last es) then
    cannotTranslate pos "operator section"
  else  
    let (_,ds) = chopBy (isOp "||") es
    in  foldr1 orOp (map disjTrans ds)
  where
  orOp e1 e2 = condTrans e1 true e2

disjTrans :: [Exp TokenId] -> Exp TokenId
disjTrans des =
  let (_,cs) = chopBy (isOp "&&") des
  in foldr1 andOp (map conjTrans cs)
  where
  andOp e1 e2 = condTrans e1 e2 false

conjTrans :: [Exp TokenId] -> Exp TokenId
conjTrans ces =
  let (ops,bs) = chopBy (isInOpList ["==","/=","<","<=",">=",">"]) ces
      args     = map listTrans bs
  in  foldr ($) (last args) (zipWith opTrans ops args)

listTrans :: [Exp TokenId] -> Exp TokenId
listTrans bes =
  let (ops,as) = chopBy (isInOpList [":","++"]) bes
      args     = map arithTrans as
  in  foldr ($) (last args) (zipWith opTrans ops args)

arithTrans :: [Exp TokenId] -> Exp TokenId
arithTrans [e] = e
arithTrans (e1 : e2 : e3 : etc) = arithTrans (opTrans e2 e1 e3 : etc)

isOp :: String -> Exp TokenId -> Bool
isOp op (ExpVarOp _ tid) = show tid == op
isOp op (ExpConOp _ tid) = show tid == op
isOp _  _                = False

isAnOp :: Exp TokenId -> Bool
isAnOp (ExpVarOp _ tid) = True
isAnOp (ExpConOp _ tid) = True
isAnOp _                = False

isInOpList :: [String] -> Exp TokenId -> Bool
isInOpList ops e = any (\op -> isOp op e) ops

opTrans :: Exp TokenId -> Exp TokenId -> Exp TokenId -> Exp TokenId
opTrans op arg1 arg2 =
  case opString op of
  "==" -> ExpApplication noPos [equal, arg1, arg2]
  "/=" -> ExpApplication noPos [noteq, arg1, arg2]
  "<"  -> condTrans (ExpApplication noPos [lesseq, arg2, arg1])
                    false true
  "<=" -> ExpApplication noPos [lesseq, arg1, arg2]
  ">=" -> ExpApplication noPos [lesseq, arg2, arg1]
  ">"  -> condTrans (ExpApplication noPos [lesseq, arg1, arg2])
                    false true
  ":"  -> ExpApplication noPos [cons,   arg1, arg2]
  "++" -> ExpApplication noPos [append, arg1, arg2]
  "+"  -> ExpApplication noPos [plus,  arg1, arg2]
  "-"  -> ExpApplication noPos [minus, arg1, arg2]
  ano  -> cannotTranslate (getPos op) (ano ++ " operator")
  where
  opString (ExpConOp _ t) = show t
  opString (ExpVarOp _ t) = show t

{-
qualTrans :: Trans (Qual TokenId)
qualTrans n (QualPatExp p e) =
  (n', QualPatExp p e')
  where
  (n', e') = expTrans n e
qualTrans n (QualExp e) =
  (n', QualExp e')
  where
  (n', e') = ( binTickBox QualBinBox `after`
               expTrans ) n e
qualTrans n (QualLet decls) =
  (n', QualLet decls')
  where
  (n', decls') = declsTrans False n decls
-}

patTrans (PatAs pos _ _) =
  cannotTranslate pos "@ pattern"
patTrans (PatWildcard pos) =
  cannotTranslate pos "_ pattern"
patTrans (PatIrrefutable pos pat) =
  cannotTranslate pos "~ pattern"
patTrans (PatNplusK pos n n' k c d) =
  cannotTranslate pos "n+k pattern"
patTrans e =
  expTrans e

cannotTranslate :: Pos -> String -> a
cannotTranslate pos msg =
  error msg

