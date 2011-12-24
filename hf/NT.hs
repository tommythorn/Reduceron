{-# OPTIONS -fno-warn-incomplete-patterns #-}
module NT ( NT(..), NewType(..), Kind(..)
	, mkNTvar, mkNTexist, mkNTcons
	, anyNT, consNT, freeNT, freshNT, polyNT, strTVar
	, sndNTvar, strNT, strictNT, transCtxs, useNT
	, contextNT, ntContext2Pair, stripNT, anyVarNT
	) where


import Id(Id)
import Extra(mixComma,mixSpace,mix)
import Char

infixr 5 :->:
data Kind = Star | Kind :->: Kind deriving (Eq,Ord)

-- Perhaps NewType is a type schema?  It quantifies variables over
-- an arrow of NTs.
data NewType = NoType
             | NewType [Id]       -- universally quantified type variables
                       [Id]       -- existentially quantified type variables
                       [(Id,Id)]  -- context (class, type variable)
                       [NT]       -- simple types 
                                  -- ex.: [Int,Char,Bool] = Int->Char->Bool
             deriving (Eq)

instance Show NewType where
  showsPrec _d (NoType) =  showString " -- no type --"
  showsPrec _d (NewType free _exist ctxs nts) = 
    showString (strTVarsCtxsNTs free ctxs nts)

data NT = NTany   Id       -- can be instantiated with unboxed 
                           -- (needed during type checking)
        | NTvar   Id Kind
        | NTexist Id Kind
        | NTstrict NT
	| NTapp   NT NT
        | NTcons  Id Kind [NT] 	-- combines constructor + application
        | NTcontext Id Id  -- context (class, type variable)
                           -- purpose here completely unclear (used?)
         deriving (Eq,Ord)

anyNT :: [Id] -> NT -> NT
contextNT :: NT -> Bool
kindNT :: NT -> Kind
mkNTcons :: Id -> [NT] -> NT
mkNTexist :: Id -> NT
mkNTvar :: Id -> NT
ntContext2Pair :: NT -> (Id, Id)
polyNT :: [Id] -> NT -> NT
sndNTvar :: (t, Id) -> (t, NT)
strTVar :: Int -> String
strTVarsCtxsNTs :: [Int] -> [(Int, Int)] -> [NT] -> String
strTVs :: [Int] -> String
strictNT :: NT -> Bool
stripNT :: NT -> Id
transCtxs ::
  (t1 -> t3) -> (t -> t2) -> [(t, t1)] -> [(t2, t3)]

mkNTvar v = NTvar v Star	-- simple tyvar
mkNTexist v = NTexist v Star	-- simple tyvar

mkNTcons i nts = NTcons i kind nts
    where kind = foldr (:->:) Star (map kindNT nts)

kindNT (NTany _) = Star
kindNT (NTvar _ k) = k
kindNT (NTexist _ k) = k
kindNT (NTapp nt1 _nt2) = case (kindNT nt1) of (_:->:k) -> k
kindNT (NTcons _ k nts) = foldl (\(_:->:k') _ -> k') k nts
kindNT (NTcontext _ _) = Star

stripNT (NTany   v)   = v
stripNT (NTvar   v _) = v
stripNT (NTexist v _) = v
stripNT (NTapp (NTvar v _) _nt ) = v
stripNT nt = error ("stripNT on " ++ show nt)

strictNT (NTstrict _) = True
strictNT _ = False

ntContext2Pair (NTcontext c a) = (c,a)

contextNT (NTcontext _ _) = True
contextNT  _ = False


{- Determine the type constructors that occur in the given type -}
consNT :: NT -> [Id]

consNT nt0 =
  consNT' nt0 []
 where
  consNT' (NTstrict nt) r = consNT' nt r
  consNT' (NTapp t1 t2) r = consNT' t1 (consNT' t2 r)
  consNT' (NTcons c _ nts) r = c:foldr consNT' r nts
  consNT' _ r = r

{- 
Same as consNT except that constructor from NTcontext goes also into result.
used only in module Export 
-}
useNT :: NT -> [Id]

useNT (NTany  _a)    = []
useNT (NTvar  _a _)  = []
useNT (NTexist _a _) = []
useNT (NTstrict t) = useNT t
useNT (NTapp t1 t2) =  useNT t1 ++ useNT t2
useNT (NTcons a _ tas) =  a:concatMap useNT tas
useNT (NTcontext c _v) =  [c]


{- Determine type variables that occur in given type. -}
freeNT :: NT -> [Id]

freeNT (NTany  a)    = [a]
freeNT (NTvar  a _)  = [a]
freeNT (NTexist a _) = [a]
freeNT (NTstrict t) = freeNT t
freeNT (NTapp t1 t2) =  freeNT t1 ++ freeNT t2
freeNT (NTcons _a _ tas) =  concat (map freeNT tas)


{- 
Exchange type variables according to given mapping in given type. 
(not existentially quantified vars.
-}
freshNT :: (Id -> Id) -> NT -> NT

freshNT tv (NTany  a)   = NTany (tv a)
freshNT tv (NTvar  a k) = NTvar (tv a) k
freshNT _tv t@(NTexist  _a _) = t
freshNT tv (NTstrict t) = {- NTstrict -}  (freshNT tv t)
freshNT tv (NTapp t1 t2) =  NTapp (freshNT tv t1) (freshNT tv t2)
freshNT tv (NTcons a k tas) =  NTcons a k (map (freshNT tv) tas)
freshNT tv (NTcontext c v) =  NTcontext c (tv v)

anyNT _av t@(NTany  _a)    = t
anyNT av t@(NTvar  a _)  = if a `elem` av then NTany a else t
anyNT _av t@(NTexist _a _) = t
anyNT av (NTstrict t) = NTstrict (anyNT av t)
anyNT av (NTapp t1 t2) =  NTapp (anyNT av t1) (anyNT av t2)
anyNT av (NTcons a k tas) =  NTcons a k (map (anyNT av) tas)

polyNT fv t@(NTany  a) = if a `elem` fv then mkNTvar a else t
polyNT _fv t@(NTvar  _a _) = t
polyNT _fv t@(NTexist _a _) = t
polyNT fv (NTstrict t) = NTstrict (polyNT fv t)
polyNT fv (NTapp t1 t2) = NTapp (polyNT fv t1) (polyNT fv t2)
polyNT fv (NTcons a k tas) = NTcons a k (map (polyNT fv) tas)

transCtxs tv tc ctxs = map (\(c,v) -> (tc c,tv v)) ctxs 



{- Show function for NT, parameterised by show functions for 
constructors/class names and for type variables.
-}
strNT :: (Int -> String) -> (Int -> String) -> NT -> String

strNT _c p (NTany  a) = p a++"#"
strNT _c p (NTvar  a _) = p a
strNT _c p (NTexist a _) = p a++"?"
strNT c p (NTstrict t) = "!" ++ strNT c p t
strNT c p (NTapp t1 t2) = "(" ++ strNT c p t1  ++ " " ++ strNT c p t2 ++ ")"
strNT c _p (NTcons a _ []) = c a
strNT c p (NTcons a _ tas) = "(" ++ c a ++ " " ++ mixSpace (map (strNT c p) tas) ++ ")"
strNT c p (NTcontext a _v) = "(" ++ c a ++ " " ++ p a ++ ") => "

instance Show NT where
  showsPrec _d nt = ((strNT show (show ::(Int->String)) nt)++)

strTVar v = let cv =  toEnum (v + fromEnum 'a')
            in if 'a' <= cv && cv <= 'z'
	       then [cv]
	       else toEnum (v`mod`26 + fromEnum 'a'):'_':show (v`div`26)
	--     else '_':show v


strCtxs ::  [(Int,Int)] -> String
strCtxs [] = ""
strCtxs ctxs = "(" ++ mixComma (map ( \ (c,v) -> show c ++ ' ':strTVar v ) ctxs) ++ ") => "

strTVs [] = ""
strTVs tvs =  "\\/ " ++ mixSpace (map strTVar tvs) ++ " . "

strTVarsCtxsNTs tvs ctxs [] =  strTVs tvs ++ strCtxs ctxs ++ " -"
strTVarsCtxsNTs tvs ctxs nts =  
  strTVs tvs ++ strCtxs ctxs ++ mix " -> " (map (strNT show strTVar) nts)


sndNTvar (c,v) = (c,mkNTvar v) -- used for ctxs		*** KIND??


anyVarNT :: NT -> Maybe Id
anyVarNT (NTany tvn) = Just tvn
anyVarNT (NTvar tvn _) = Just tvn
anyVarNT _ = Nothing
