{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-deprecations #-}
{- ---------------------------------------------------------------------------
Central data structures of the symbol table
-}
module Info(module Info, IdKind,TokenId,NewType,InfixClass(..),Pos
           ,AssocTree,Tree) where

import IdKind(IdKind)
import TokenId(TokenId)
import NT
import Extra(Pos,sndOf,strace)
import SysDeps(PackedString)
import AssocTree
import Syntax(InfixClass(..))
import Id(Id)

data IE = IEnone | IEsel | IEsome | IEabs | IEall deriving (Eq,Show) 
-- This is "Interface Exports"
--    defined in a lattice   IEall
--                          /     \
--                         |     IEsome
--                       IEsel     |
--                         |     IEabs
--                          \     /
--                           IEnone
--   IEall  -> exported (with all constructors/fields/methods)
--   IEsome -> exported with selected constructors/fields/methods
--   IEabs  -> exported abstractly (without constructors/fields/methods)
--   IEnone -> not exported
--   IEsel  -> selected constructors/fields/methods
--			 (is exported, despite defn below!)

isExported :: IE -> Bool
isExported IEnone = False
isExported IEsel  = False
isExported _      = True

combIE :: IE -> IE -> IE
combIE IEall  _     = IEall
combIE _      IEall = IEall
combIE IEnone i     = i
combIE i     IEnone = i
combIE IEsome IEabs = IEsome
combIE IEabs IEsome = IEsome
combIE _      i     = i

-- Patch newtype for exports  (Its constructor must always be in the
-- interface file, even if not visible in the importing module.)
patchIE :: IE -> IE
patchIE IEabs = IEsome
patchIE ie    = ie

data DataKind = 
    DataTypeSynonym Bool  -- True <-> unboxed after expansion
                    Int   -- depth (used to determine 
                          -- which type synonym to expand)
  | DataNewType	    Bool  -- always False
                    [Id]  -- constructor(one or zero) 
  | Data 	    Bool  -- True <-> unboxed
                    [Id]  -- constructors
  | DataPrimitive   Int	  -- size
  deriving (Show)

data Info =
    InfoClear   -- used to remove imported when redefining in mutally 
                -- recursive modules and when compiling the prelude
  | InfoUsed      Id       -- unique
                  [(IdKind,TokenId,PackedString,Pos)] -- occurrence where used
  | InfoUsedClass Id       -- unique
                  [(IdKind,TokenId,PackedString,Pos)] -- occurrence where used
                  (AssocTree Int ([Int],[(Int,Int)]))  
                  -- instances of the class
                  -- the tree associates a type constructor with
                  -- the free variables and the superclass context 
                  -- of an instance
  | InfoData      -- data type (algebraic, type synonym, ...)
                  Id       -- unique
                  TokenId  -- token of data type name
                  IE       -- is exported?
                  NewType  -- if type synonym: type it is defined to be
                           -- if data or newtype: defined type
                           -- e.g.: data Num a => Test a b = A a | B b
                           -- NewType [1,2] [] [(NumId, 1)] 
                           --   [NTvar 1 Star, NTvar 2 Star, mkNTcons TestId 
                           --                     [NTvar 1 Star, NTvar 2 Star]]
                  DataKind -- kind of data type 
  | InfoClass     Int      -- unique
                  TokenId  -- token of class name
                  IE       -- is exported?
                  NewType  -- pseudo type built from class and type variable
                           -- (type of dictionary?)
                  [Id]     -- method ids refering to type declaration
                  [Id]     -- method ids refering to default definition
                           -- ids in same position refer to same method
                           -- => lists have same lengths
                  (AssocTree Int ([Int],[(Int,Int)]))    
                  -- instances of the class
                  -- the tree associates a type constructor with
                  -- the free variables and the superclass context 
                  -- of an instance
  | InfoVar       -- term variable 
                  Int          -- unique 
                  TokenId      -- token for name
                  IE           -- is exported?
                  (InfixClass TokenId,Int)  -- fixity 
                  NewType      -- type
                  (Maybe Int)  -- arity (if available)
  | InfoConstr    -- data constructor
                  Int          -- unique 
                  TokenId      -- token for name
                  IE           -- is exported?
                  (InfixClass TokenId,Int)  -- fixity 
                  NewType      -- type of the constructor
                  [Maybe Int]  -- field names (if they exist) 
                  Int          -- data type to which constructor belongs
  | InfoField     -- field name
                  Id           -- unique
                  TokenId      -- token for name
                  IE           -- is exported?
                  [(Id,Int)]   -- [(data constructor, offset for this constr.)]
                  Id           -- iData
                  Id           -- iSel	
                  -- unique tid [(constructor,offset)] type selector
  | InfoMethod    -- for type declaration of method in a class definition
                  Id           -- unique 
                  TokenId      -- token for method name
                  IE           -- is exported?
                  (InfixClass TokenId,Int) -- fixity
                  NewType 
                  (Maybe Int)  -- arity (if available; here bogus)
                  Id           -- unique of class to which method belongs
  | InfoIMethod   -- for definition in instance definition
                  Id           -- unique 
                  TokenId      -- token for name
                  NewType 
                  (Maybe Int)  -- arity (if available) 
                  Id           -- iMethod (0 after renaming)
                  -- The type is NewType free instancs_ctx instance_type, 
                  -- for real type follow iMethod
  | InfoDMethod   -- for default definition in class definition
                  Id           -- unique
                  TokenId      -- token for method name
                  NewType 
                  (Maybe Int)  -- arity (if available) 
                  Id           -- class to which method belongs
  | InfoInstance  -- Only used in Export
                  Id           -- unique
                  NewType 
                  Id 	       -- unique of class (of which this is instance)
  | InfoName      Id           -- unique
                  TokenId      -- token for name
                  Int          -- arity
                  TokenId      
                  Bool         --PHtprof indicates subfn
    -- inserted late to hold name and arity for some functions 
    -- (second TokenId is profname )
  deriving (Show)

{- Template
z (InfoUsed   unique uses) =
z (InfoUsedClass unique uses insts) =
z (InfoData   unique tid ie nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) ->
	(DataNewType unboxed constructors) ->
	(Data unboxed  constrs) ->
	(DataPrimitive size) ->
z (InfoClass  unique tid ie nt ms ds insts) = 
z (InfoVar    unique tid ie fix nt annot) = 
z (InfoConstr unique tid ie fix nt fields iType) =
z (InfoField  unique tid ie icon_offs iData iSel) =
z (InfoMethod unique tid ie fix nt annot iClass) =
z (InfoIMethod unique tid nt annot iMethod) =
z (InfoDMethod unique tid nt annot iClass) =
z (InfoInstance unique  nt iClass) =
z (InfoName pos unique tid Int ptid subfn) =  --PHtprof
-}

clearI :: a -> Info
clearI _ = InfoClear


--isClear InfoClear = True
--isClear _ = False

isMethod :: Info -> Bool
isMethod (InfoMethod _unique _tid _ie _fix _nt _annot _iClass) = True
isMethod _ = False


isData :: Info -> Bool 
isData (InfoData   _unique _tid _exp _nt _dk) = True
isData _ = False


isRealData :: Info -> Bool
isRealData (InfoData   _unique _tid _exp _nt dk) =
      case dk of
	(DataTypeSynonym _unboxed _depth) -> False
	(DataNewType _unboxed _constructors) -> False
	(DataPrimitive _size) -> True
	(Data _unboxed  _constrs) -> True
isRealData info = error ("isRealData " ++ show info)


isRenamingFor :: AssocTree Int Info -> Info -> NewType
isRenamingFor _st (InfoData  _unique _tid _exp nt (DataTypeSynonym _ _depth)) = nt
isRenamingFor st info@(InfoData  _unique _tid _exp _nt (DataNewType _ constrs)) =
    case constrs of
      []  -> error ("Problem with type of a foreign imported function:\n"
                    ++"Cannot find constructor for newtype: "++show info)
      [c] -> case lookupAT st c of
               Just i  -> ntI i
               Nothing -> error ("Cannot find info for newtype constructor: "
                                 ++show info)
isRenamingFor _st info = error ("isRenamingFor " ++ show info)


isDataUnBoxed :: Info -> Bool  
isDataUnBoxed (InfoData _unique _tid _exp _nt dk) =
      case dk of
	(DataTypeSynonym unboxed _depth) -> unboxed
	(DataNewType unboxed _constructors) -> unboxed
	(Data unboxed  _constrs) -> unboxed
	(DataPrimitive _size) -> True
isDataUnBoxed info = error ("isDataUnBoxed: " ++ show info)


isField :: Info -> Bool
isField (InfoField _ _ _ _ _ _) = True
isField _ = False

isClass :: Info -> Bool
isClass (InfoClass _ _ _ _ _ _ _) = True
isClass _ = False

isUsedClass :: Info -> Bool
isUsedClass (InfoUsedClass _ _ _) = True
isUsedClass _ = False


depthI :: Info -> Maybe Int
depthI (InfoData _unique _tid _exp _nt dk) =
    case dk of
	(DataTypeSynonym _unboxed depth) -> Just depth
	_ -> Nothing
depthI _ = Nothing


typeSynonymBodyI :: Info -> Maybe NewType
typeSynonymBodyI (InfoData _ _ _ nt (DataTypeSynonym _ _)) = Just nt
typeSynonymBodyI _ = Nothing


updTypeSynonym :: Bool -> Int -> Info -> Info
updTypeSynonym unboxed depth (InfoData unique tid isExp nt dk) =
      case dk of
	(DataTypeSynonym _ _) ->
	  (InfoData unique tid isExp nt (DataTypeSynonym unboxed depth)) 


{-
-- Sets the unboxedness information in newtype info as given.
-}
updNewType :: Bool -> Info -> Info
updNewType unboxed (InfoData unique tid isExp nt dk) =
      case dk of
	(DataNewType _ constructors) -> 
          InfoData unique tid isExp nt (DataNewType unboxed constructors)

{-
-- Sets the type information in variable info as given.
-- Is only applied to identifiers without types,i.e. never methods of any kind!
-}
newNT :: NewType -> Info -> Info
newNT nt (InfoVar unique tid ie fix _ annot) =
          InfoVar unique tid ie fix nt annot


ntI :: Info -> NewType
ntI (InfoData     _unique _tid _ie nt _dk)               = nt
-- ntI (InfoClass unique tid ie nt ms ds)            = nt   --- Not needed?
ntI (InfoVar      _unique _tid _ie _fix nt _annot)        = nt
ntI (InfoConstr   _unique _tid _ie _fix nt _fields _iType) = nt
ntI (InfoMethod   _unique _tid _ie _fix nt _annot _iClass) = nt
ntI (InfoIMethod  _unique _tid nt _annot _iMethod)       = nt  -- Work here?
ntI (InfoDMethod  _unique _tid nt _annot _iClass)        = nt


strictI :: Info -> [Bool]
strictI (InfoConstr _ _ _ _ (NewType _free [] _ctx nts) _ _) = 
    map strictNT (init nts)
strictI _ = []
  -- Not strict in any argument so it doesn't matter if we return empty list

qDefI :: Info -> Bool
qDefI (InfoUsed _ _) = False
qDefI (InfoUsedClass _ _ _) = False
qDefI _ = True

uniqueI :: Info -> Id
uniqueI (InfoUsed   unique _)             = unique
uniqueI (InfoUsedClass unique _ _)        = unique
uniqueI (InfoData   unique _tid _ie _nt _dk)  = unique
uniqueI (InfoClass  unique _ _ _ _ _ _)   = unique
uniqueI (InfoVar     unique _ _ _ _ _)    = unique
uniqueI (InfoConstr  unique _ _ _ _ _ _)  = unique
uniqueI (InfoField   unique _ _ _ _ _)    = unique
uniqueI (InfoMethod  unique _ _ _ _ _ _)  = unique
uniqueI (InfoIMethod  unique _ _ _ _)     = unique
uniqueI (InfoDMethod  unique _ _ _ _)     = unique
uniqueI (InfoInstance unique _ _)         = unique
uniqueI (InfoName  unique _ _ _ _)        = unique --PHtprof


tidI :: Info -> TokenId
tidI (InfoData   _unique tid _exp _nt _dk) = tid
tidI (InfoClass  _u tid _ _ _ _ _)      = tid
tidI (InfoVar     _u tid _ _ _ _)       = tid
tidI (InfoConstr  _u tid _ _ _ _ _)     = tid
tidI (InfoField   _u tid _ _ _ _)       = tid
tidI (InfoMethod  _u tid _ _ _ _ _)     = tid
tidI (InfoIMethod  _u tid _ _ _)        = tid
tidI (InfoDMethod  _u tid _ _ _)        = tid
tidI (InfoName  _u tid _ _ _)           = tid --PHtprof
tidI (InfoUsedClass _u ((_,tid,_,_):_) _) = tid	--MW
tidI info = error ("tidI (Info.hs) called with bad info:\n" ++ show info)


cmpTid :: TokenId -> Info -> Bool
cmpTid _t (InfoUsed _ _)        = False
cmpTid _t (InfoUsedClass _ _ _) = False
cmpTid t i                     = tidI i == t


methodsI :: Info -> [(Int,Int)]
methodsI (InfoClass _u _tid _ie _nt ms ds _inst) = zip ms ds


instancesI :: Info -> AssocTree Int ([Int],[(Int,Int)])
instancesI (InfoClass _u _tid _e _nt _ms _ds inst) = inst
instancesI info@(InfoUsedClass _u _uses inst) = 
  strace ("***instanceI(1) "++show info++"\n") inst
instancesI info = 
  strace ("***instanceI(2) "++show info++"\n") initAT 
  -- This is a lie!!! For some reason has this class no real entry


{- Return identifiers of all superclasses of the class which is described
-- by given info.
-}
superclassesI :: Info -> [Int]
superclassesI (InfoClass _ _ _ (NewType _ [] ctxs _) _ _ _) = map fst ctxs
superclassesI info = error ("superclassesI " ++ show info)

{- Add information about an instance to info of a class.
-- If information about this instance exists already in info, then info left
-- unchanged.
-- type constructor -> free type variables -> context -> class info
--             -> class info
-}
addInstanceI :: Int -> [Int] -> [(Int,Int)] -> Info -> Info
addInstanceI con free ctxs info@(InfoClass u tid e nt ms ds inst) =
  case lookupAT inst con of
    Just _ -> info
    Nothing -> InfoClass u tid e nt ms ds (addAT inst sndOf con (free,ctxs))
addInstanceI con free ctxs info@(InfoUsedClass u uses inst) =
  case lookupAT inst con of
    Just _ -> info
    Nothing -> InfoUsedClass u uses (addAT inst sndOf con (free,ctxs))
addInstanceI con free ctxs (InfoUsed u uses) = 
	addInstanceI con free ctxs (InfoUsedClass u uses initAT)

{-
-- In joining two trees for describing instances the second one gets
-- precedence in case of conflict.
-}
joinInsts :: AssocTree Int a -> AssocTree Int a -> AssocTree Int a
joinInsts inst0 inst' =
  foldr (\(k,v) inst -> addAT inst sndOf k v) inst0 (listAT inst')


{- Determine constructors of a type from the info of the type -}
constrsI :: Info -> [Int]
constrsI (InfoName  unique _tid _i _ptid _) = [unique]   --PHtprof
  -- ^this is a lie! but it is consistent with belongstoI :-)
constrsI (InfoData   _unique tid _exp _nt dk) =
      case dk of
	(DataTypeSynonym _unboxed _depth) ->  
           strace ("Constr of type synonym "++show tid++"\n") []
	(DataNewType _unboxed constructors) -> constructors
	(DataPrimitive _size) ->
           strace ("Constr of data primitive "++show tid++"\n") []
	(Data _unboxed  constrs) -> constrs
constrsI info = error ("constrsI " ++ show info)


updConstrsI :: Info -> [Int] -> Info
updConstrsI (InfoData unique tid isExp nt dk) constrs' =
  case dk of
    (Data unboxed  _constrs) -> 
      InfoData unique tid isExp nt (Data unboxed constrs')


fieldsI :: Info -> [Maybe Int]
fieldsI (InfoConstr _unique _tid _ie _fix _nt fields _iType) = fields


combInfo :: Info -> Info -> Info

combInfo  InfoClear      info'               = info'
combInfo (InfoUsed _ w) (InfoUsed u' w')     = InfoUsed u' (w++w')
combInfo (InfoUsed _ _)  info'               = info'
combInfo  info           InfoClear           = info
combInfo  info          (InfoUsed _ _)       = info
combInfo (InfoUsedClass _ _uses insts) 
         (InfoClass u tid isExp nt ms ds insts') =
  InfoClass u tid isExp nt ms ds (joinInsts insts' insts)
combInfo (InfoClass _ tid isExp nt ms ds insts) 
         (InfoUsedClass u _uses insts') =
  InfoClass u tid isExp nt ms ds (joinInsts insts' insts)
combInfo (InfoClass u tid isExp nt ms ds insts) 
         (InfoClass _u' _tid' isExp' _nt' [] [] insts') =	
  InfoClass u tid (combIE isExp isExp') nt ms ds (joinInsts insts' insts)
combInfo (InfoClass u tid isExp _nt _ms _ds insts) 
         (InfoClass _u' _tid' isExp' nt' ms' ds' insts') =	
  InfoClass u tid (combIE isExp isExp') nt' ms' ds' (joinInsts insts' insts)
combInfo info@(InfoData _u _tid _isExp _nt _dk) 
         info'@(InfoData _u' _tid' isExp' _nt' dk')  =
  case dk' of
    Data _unboxed [] -> info
    _ -> if isExported isExp' then info' else info
combInfo info info' =  
  -- Use new (if possible) so that code can override old imported
	if isExported (expI info)
	then info
        else info'

expI :: Info -> IE
expI (InfoData    _ _ ie _ _)      = ie
expI (InfoClass   _ _ ie _ _ _ _)  = ie
expI (InfoVar     _ _ ie _ _ _)    = ie
expI (InfoConstr  _ _ ie _ _ _ _)  = ie
expI (InfoField   _ _ ie _ _ _)    = ie  -- Data contains export info
expI (InfoMethod  _ _ ie _ _ _ _)  = ie
expI (InfoIMethod _ _ _ _ _)       = IEnone
expI (InfoDMethod _ _ _ _ _)       = IEnone
expI _info                          = IEnone  -- I get InfoUsed here !!!

-- arity without context (Visible)
arityVI :: Info -> Int
arityVI (InfoVar _ _ _ _ _ (Just arity))             =  arity
arityVI (InfoConstr _ _ _ _ (NewType _ _ _ nts) _ _) = length nts - 1
arityVI (InfoMethod _ _ _ _ _ (Just _arity) _)        = 1
arityVI (InfoIMethod _ _ _ (Just arity) _)           = arity
arityVI (InfoDMethod _ _ _ (Just arity) _)           = arity 
arityVI (InfoName _ _ arity _ _)                     = arity --PHtprof

-- arity with context
arityI :: Info -> Int
arityI (InfoVar _ _ _ _ (NewType _ _ ctx _) (Just arity))  = length ctx + arity
arityI (InfoVar _ _ _ _ _                   (Just arity))  = arity
					-- NR Generated after type deriving
arityI (InfoConstr _ _ _ _ (NewType _ _ _ nts) _ _)        = length nts - 1
arityI (InfoMethod _ _ _ _ _ (Just _arity) _)               = 1
-- Wrong !!! 
-- arityI  (InfoIMethod _ _ (NewType _ _ ctx _) (Just arity) _)
--                                                         = length ctx + arity
arityI (InfoDMethod  _ _  (NewType _ _ ctx _) (Just arity) _)
                                                           = length ctx + arity
                                                             + 1
					--  +1 is for the dictionary
arityI (InfoName  _unique _tid arity _ptid _)                 = arity --PHtprof
arityI info  =  error ("arityI " ++ show info)

arityIM :: Info -> Int
arityIM (InfoMethod _ _ _ _ (NewType _ _ ctx _) (Just arity) _)
                                                           = length ctx + arity

fixityI :: Info -> (InfixClass TokenId, Int)
fixityI (InfoVar     _unique _tid _ie fix _nt _annot)        = fix
fixityI (InfoConstr  _unique _tid _ie fix _nt _fields _iType) = fix
fixityI (InfoMethod  _unique _tid _ie fix _nt _annot _iClass) = fix
fixityI _ = (InfixDef,9::Int)


belongstoI :: Info -> Id
belongstoI (InfoConstr  _unique _tid _ie _fix _nt _fields iType)  = iType
belongstoI (InfoField   _unique _tid _ie _icon_offs iData _iSel) = iData
belongstoI (InfoMethod  _unique _tid _ie _fix _nt _annot iClass)  = iClass
belongstoI (InfoIMethod  _unique _tid _nt _annot iMethod)       = iMethod  
  -- ^Maybe ought to be it's own function
belongstoI (InfoDMethod  _unique _tid _nt _annot iClass)        = iClass
belongstoI (InfoInstance _unique  _nt iClass)                 = iClass
belongstoI (InfoName  unique _tid _i _ptid _)                  = unique  --PHtprof
  -- ^this is a lie! but it is consistent with constrsI :-)
belongstoI info =  error ("belongstoI " ++ show info)


profI :: Info -> TokenId
profI (InfoData   _unique tid _exp _nt _dk) = tid
profI (InfoClass  _u tid _ _ _ _ _)      = tid
profI (InfoVar     _u tid _ _ _ _)       = tid
profI (InfoConstr  _u tid _ _ _ _ _)     = tid
profI (InfoField   _u tid _ _ _ _)       = tid
profI (InfoMethod  _u tid _ _ _ _ _)     = tid
profI (InfoIMethod  _u tid _ _ _)        = tid
profI (InfoDMethod  _u tid _ _ _)        = tid
profI (InfoName  _u _tid _ ptid _)        = ptid --PHtprof
profI info = error ("profII (Info.hs) " ++ show info)
