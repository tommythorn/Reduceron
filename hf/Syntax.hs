{-# OPTIONS -fno-warn-deprecations #-}
module Syntax(module Syntax, Pos, TokenId) where

import Extra(Pos,strChr,strStr)
import SysDeps(PackedString)
import TokenId(TokenId)
import Id(Id)
import Ratio
import Maybe(isNothing,fromJust)

{-
Note that some syntactic constructs contain the syntactic construct 
"Type". However, the rename pass replaces this representation by the internal
type representation "NewType" and "NT". So the syntactic constructs that
use "Type" are removed by the renaming pass or the type representation is only
half translated (TokenId -> Id). Are the latter still used later?

It probably would have been better if the whole syntax had been parameterised
with respect to the type representation; but such an additional parameter 
would also be tiresome.
-}

data Module id =
--     module modid [export] where { impdecls; fixdecls; topdecls }
       Module Pos id (Maybe [Export id]) [ImpDecl id] [FixDecl id] (Decls id)

data Export id =
       ExportEntity  Pos (Entity id)  -- pos superfluous, same in entity?
     | ExportModid   Pos id

data ImpDecl id =
--     import ?qualified? modid ?as modid? ?hiding? (import,..)?
       Import    (Pos,id) (ImpSpec id)
     | ImportQ   (Pos,id) (ImpSpec id)
     | ImportQas (Pos,id) (Pos,id) (ImpSpec id)
     | Importas  (Pos,id) (Pos,id) (ImpSpec id)

importedModule :: ImpDecl a -> a
importedModule (Import (_,impDecl) _) = impDecl 
importedModule (ImportQ (_,impDecl) _) = impDecl
importedModule (ImportQas (_,impDecl) _ _) = impDecl 
importedModule (Importas (_,impDecl) _ _) = impDecl


data ImpSpec id =
       NoHiding [Entity id]
     | Hiding   [Entity id]

data Entity id =
       EntityVar        Pos id             -- varid
     | EntityConClsAll  Pos id             -- TyCon(..) | TyCls(..)
     | EntityConClsSome Pos id [(Pos,id)]
	  -- TyCon | TyCls | TyCon(conid,..,conid) | TyCls(varid,..,varid) 

data InfixClass a =
                  InfixDef
                | InfixL
                | InfixR 
                | Infix
                | InfixPre a

instance Eq (InfixClass a) where
        InfixDef   == InfixDef   = True
        InfixL     == InfixL     = True
        InfixR     == InfixR     = True
        Infix      == Infix      = True
        (InfixPre _) == (InfixPre _)  = True
        _          == _          = False

instance (Show a) => Show (InfixClass a) where
  showsPrec _d InfixDef =  showString "infixl{-def-} "
  showsPrec _d InfixL   =  showString "infixl "
  showsPrec _d InfixR   =  showString "infixr "
  showsPrec _d Infix    =  showString "infix  "
  showsPrec _d (InfixPre a) =  showString "prefix " . shows a . showChar ' '


type FixDecl id = (InfixClass id,Int,[FixId id])

data FixId a =
   FixCon Pos a
 | FixVar Pos a

stripFixId :: FixId a -> a
stripFixId (FixCon _ a) = a
stripFixId (FixVar _ a) = a


data Decls id =
   DeclsParse [Decl id]
 | DeclsScc [DeclsDepend id]

-- used very often:
noDecls :: Decls id
noDecls = DeclsParse []


data DeclsDepend id =
       DeclsNoRec (Decl id)
     | DeclsRec   [Decl id]

data Decl id =
       -- | for type synonym: type   simple  = type
       DeclType (Simple id) (Type id)
       -- | renamer replaces DeclType by this.
       --   the type is in the symbol table, referenced by Id
     | DeclTypeRenamed Pos Id   -- intentionally not "id"

       -- | {Nothing = newtype, Just False = data, Just True = data unboxed}
       --   context => simple = constrs 
       --   deriving (tycls)
     | DeclData (Maybe Bool) [Context id] (Simple id) [Constr id] [(Pos,id)]
       -- | data primitive conid size
     | DeclDataPrim Pos id Int
       -- | Introduced by Rename to mark that we might need 
       --   to generate selector functions
       --       position data/dataprim [(field,selector)]
     | DeclConstrs Pos id [(Pos,id,id)] 
       -- | class context => class where { signatures/valdefs; }
       --   position, context, class, type variables, fundeps, method decls
     | DeclClass Pos [Context id] id [id] [FunDep id] (Decls id)
       -- | instance context => tycls inst where { valdefs }
     | DeclInstance Pos [Context id] id [Instance id] (Decls id)
       -- | default (type,..)
     | DeclDefault [Type id]
       -- | var primitive arity :: type
     | DeclPrimitive Pos id Int (Type id)
       -- | foreign import [callconv] [extfun] [unsafe|cast|noproto] var :: type
       --                                   (final id part is wrapper-fn for IO)
       --               callconv extfun intfun arity [u|c|n] ty wrapperId
     | DeclForeignImp Pos CallConv String id Int Safety (Type id) id
       -- | foreign export  callconv  [extfun]  var :: type
     | DeclForeignExp Pos CallConv String id (Type id)
       -- | vars :: context => type
     | DeclVarsType [(Pos,id)] [Context id] (Type id)
     | DeclPat (Alt id)
     | DeclFun Pos id [Fun id] -- "var = ..." is a DeclFun, not a DeclPat
--   | DeclSelect id Int id  
       -- ^ introduced with pattern elimination (id = select Int id)
--     Used for unimplemented things
     | DeclIgnore String
     | DeclError String
     | DeclAnnot (Decl id) [Annot id]
--   | DeclPragma String String
     -- | infix[rl] int id,..,id
     | DeclFixity (FixDecl id)

-- for foreign imports/exports
data Safety = Unsafe | Safe
instance Show Safety where
  showsPrec _ Unsafe     = showString "unsafe"
  showsPrec _ Safe       = id

-- supported foreign calling conventions
data CallConv = C | Cast | Noproto | Haskell | Other String deriving Eq
instance Show CallConv where
  showsPrec _ C         = showString "ccall"
  showsPrec _ Cast      = showString "cast"
  showsPrec _ Noproto   = showString "noproto"
  showsPrec _ Haskell   = showString "haskell"
  showsPrec _ (Other s) = showString s


data ClassCode ctx id = -- introduced by RmClasses
   CodeClass Pos id  -- class id
 | CodeInstance Pos id id [id] [ctx] [id]  
   -- class id, typ id, args, ctxs, method ids

-- | We parse MPTC with functional dependencies, only for hat-trans.
data FunDep id = [id] :->: [id]


data Annot id = AnnotArity (Pos,id) Int
              | AnnotPrimitive (Pos,id) PackedString
              | AnnotNeed [[id]]
              | AnnotUnknown

--                 lhs pats, guarded exprs,   local defs
data Fun id = Fun  [Pat id] (Rhs id) (Decls id)

funArity :: Fun id -> Int
funArity = length . \(Fun ps _ _) -> ps

data Alt id = Alt  (Pat id) (Rhs id) (Decls id)

data Rhs id = Unguarded (Exp id)
            | Guarded [(Exp id,Exp id)]  -- the list has at least one element

data Type id =
       TypeCons         Pos id [Type id]
     | TypeApp          (Type id) (Type id)
     | TypeVar          Pos id
     | TypeStrict       Pos (Type id)

data Sig id = Sig [(Pos,id)] (Type id)  -- for interface file?

data Simple id = Simple Pos id [(Pos,id)]

simpleToType :: Simple id -> Type id
simpleToType (Simple pos tcId pargs) = 
  TypeCons pos tcId (map (TypeVar pos . snd) pargs)

data Context id = Context Pos id [(Pos,id)]


{-
Data constructor applied to type variables, possibly with field names.
As appearing on right hand side of data or newtype definition.
-}
-- ConstrCtx is always used if forall is specified
-- the intention is to remove Constr completely when all of nhc13 
-- have been updated 
data Constr id = Constr 
                   Pos       -- position of data constructor
                   id        -- data constructor
                   [(Maybe [(Pos,id)],Type id)]
                   -- argumentlist with field labels if any
                   -- (many field labels with same type possible)
                   -- the type admits impossible arguments:
                   -- either all arguments have field names or none
               | ConstrCtx  
                   [(Pos,id)]     -- type variabes from forall
                   [Context id]   -- context of data constructor
                   Pos 
                   id        
                   [(Maybe [(Pos,id)],Type id)]

getConstrId :: Constr id -> id
getConstrId (Constr _ id0 _) = id0
getConstrId (ConstrCtx _ _ _ id0 _) = id0

getConstrArgumentList :: Constr id -> [(Maybe [(Pos,id)],Type id)]
getConstrArgumentList (Constr _ _ xs) = xs
getConstrArgumentList (ConstrCtx _ _ _ _ xs) = xs

getConstrLabels :: Constr id -> [(Pos,id)]
getConstrLabels constr =
  if null args || (isNothing . fst . head) args 
    then []
    else concatMap (fromJust . fst) args
  where
  args = getConstrArgumentList constr

getConstrArgumentTypes :: Constr id -> [Type id]
getConstrArgumentTypes constr = 
  concat . map (\(l,t) -> replicate (times l) t) . getConstrArgumentList $ 
    constr
  where
  times Nothing = 1
  times (Just labels) = length labels 

constrArity :: Constr id -> Int
constrArity = length . getConstrArgumentTypes


type Instance id = Type id  -- Not TypeVar


{-
The following is ismorphic to the type constructor Qual.
Possibly Stmt should be removed and its usage replaced everywhere by Qual.
-}
data Stmt id =
    StmtExp  (Exp id)		-- exp
  | StmtBind (Exp id) (Exp id)	-- pat <- exp
  | StmtLet (Decls id)		-- let { decls ; }


type Pat id = Exp id

data Exp id =  -- used both for expressions and patterns
      ExpScc            String (Exp id) 
      -- ^ never used! should probably be removed
    | ExpDict           (Exp id)         -- hack to mark dictionary arguments
    | ExpLambda         Pos [(Pat id)] (Exp id)  -- \ pat ... pat -> exp
    | ExpLet            Pos (Decls id) (Exp id)  -- let { decls ; } in exp
    | ExpDo             Pos [Stmt id]            -- do { stmts ; }
    | ExpCase           Pos (Exp id) [Alt id]    -- case exp of { alts; }
    | ExpFatbar         (Exp id) (Exp id)
      -- ^ never used! should probably be removed
    | ExpFail
      -- ^ never used! should probably be removed
    | ExpIf             Pos (Exp id) (Exp id) (Exp id) 	
                        -- if exp then exp else exp
    | ExpType           Pos (Exp id) [Context id] (Type id)
                        -- exp :: context => type
--- Above only in expressions, not in patterns
    | ExpRecord	        (Exp id) [Field id]
    | ExpApplication    Pos [Exp id] -- always at least two elements?
    | ExpVar            Pos id
    | ExpCon            Pos id
    | ExpInfixList      Pos [Exp id] -- Temporary, introduced by parser because
    | ExpVarOp          Pos id       -- it does not know precedence and 
    | ExpConOp          Pos id       -- associativity; removed by rename
    | ExpLit            Pos (Lit Boxed)
    | ExpList           Pos [Exp id]
-- next two are sugared lists; introduced for hpc-trans
    | ExpListComp       Pos (Exp id) [Qual id]
    | ExpListEnum       Pos (Exp id) (Maybe (Exp id)) (Maybe (Exp id))
-- and bracketed expression again for hpc-trans (need accurate pos)
    | ExpBrack          Pos (Exp id)
--- after typechecker
    | Exp2              Pos id id  -- e.g.   Ord.Eq      or Eq.Int
--- Below only in patterns
    | PatAs             Pos id (Pat id)
    | PatWildcard       Pos
    | PatIrrefutable    Pos (Pat id)
-- idea: f (n+k) = exp[n]
--  =>   f n' | k <= n' = exp[n]
--         where n = n'-k 
-- (n+k) pattern - store:   n  n' k        (k<=n')  (n'-k)
    | PatNplusK		Pos id id (Exp id) (Exp id) (Exp id)

data Field id = FieldExp  Pos id (Exp id)
              | FieldPun  Pos id     -- H98 removes (retained for error msgs)

data Boxed = Boxed | UnBoxed

instance Eq Boxed where
  Boxed == Boxed = True
  UnBoxed == UnBoxed = True
  _     == _       = False

instance Show Boxed where
  showsPrec _d Boxed   = id
  showsPrec _d UnBoxed = showChar '#'


data Lit boxed =
      LitInteger  boxed Integer
    | LitRational boxed Rational
    | LitString   boxed String
    | LitInt      boxed Int
    | LitDouble   boxed Double
    | LitFloat    boxed Float
    | LitChar     boxed Char

instance (Eq b) => Eq (Lit b) where
     a == a' = litEqual a a'   -- litEqual needed in Symbols to force correct type in gofer

litEqual :: Eq b => Lit b -> Lit b -> Bool
litEqual (LitInteger  b i) (LitInteger  b' i') = i == i' && b == b'
litEqual (LitRational b i) (LitRational b' i') = i == i' && b == b'
litEqual (LitString   b s) (LitString   b' s') = s == s' && b == b'
litEqual (LitInt      b i) (LitInt      b' i') = i == i' && b == b'
litEqual (LitDouble   b f) (LitDouble   b' f') = f == f' && b == b'
litEqual (LitFloat    b f) (LitFloat    b' f') = f == f' && b == b'
litEqual (LitChar     b c) (LitChar     b' c') = c == c' && b == b'
litEqual _                  _            = False

instance (Show b) => Show (Lit b) where
  showsPrec d lit = litshowsPrec d lit  -- litshowsPrec needed in Symbols to force correct type in gofer


litshowsPrec :: (Show t) => Int -> Lit t -> String -> String
litshowsPrec d (LitInteger  b i) = showParen (i<0) (showsPrec d i) . shows b
litshowsPrec d (LitRational b i) = 
  -- this is a hack to show a rational in floating point representation
  -- precision might be lost
  -- therer is no library function to print a rational in full precision
  -- in floating point representation
  showParen (i<0) (showsPrec d ((fromRational i)::Double)) . shows b
litshowsPrec _d (LitString b str)= showString (strStr str) . shows b
litshowsPrec d (LitInt    b i)  = showParen (i<0) (showsPrec d i) . shows b
litshowsPrec d (LitDouble b f)  = showParen (f<0) (showsPrec d f) . shows b
litshowsPrec d (LitFloat  b f)  = showParen (f<0) (showsPrec d f) . shows b
litshowsPrec _d (LitChar   b chr)= showString (strChr chr). shows b

data Qual id =
--       pat <- exp
      QualPatExp (Pat id) (Exp id)
--       pat
    | QualExp (Exp id)
--	 let decls
    | QualLet (Decls id)


--------------------

data Interface id = 
--      interface modid where {iimpdecl; fixdecl; itopdecl }
       Interface Pos id [IImpDecl id] [FixDecl id] (IDecls id)

type IImpDecl id = ImpDecl id -- No Hiding in ImpSpec

type IDecls id = Decls id -- No Valdef

