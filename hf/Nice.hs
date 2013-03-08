{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-deprecations #-}
{- ---------------------------------------------------------------------------
Converts internal types (NewType) into a nice printable string
-}
module Nice (fixTid, mkAL, mkALNT
        , niceCtxs, niceField, niceInt, niceNT
        , niceNewType, niceTid, showsOp, showsVar
        )  where

import NT
import IntState
import Extra(dropJust,mixComma,mixSpace,assocDef)
import Data.ByteString(ByteString)
import TokenId
import Id(Id)

niceNT :: Maybe ByteString  -- ??
        -> IntState           -- symboltable needed for names of ids
        -> [(Id,String)]      -- renaming mapping for type variables
        -> NT                 -- the type to be converted
        -> String

niceNT _ _state al (NTany  a) = assocDef al ('?':show a ++ "?") a
niceNT _ _state al (NTvar  a _) = assocDef al ('?':show a ++ "?") a
niceNT _ _state al (NTexist a _) = assocDef al ('?':show a ++ "?") a
niceNT m state al (NTstrict t) = "!" ++ niceNT m state al t
niceNT m state al (NTapp t1 t2) =
   '(':  niceNT m state al t1 ++ ' ': niceNT m state al t2 ++ ")"
niceNT m state _al (NTcons a _ []) = niceInt m state a ""
niceNT m state al (NTcons a _ tas0) =
        case (tidI . dropJust .  lookupIS state) a of
          TupleId _ -> '(' : mixComma (map (niceNT m state al) tas0) ++ ")"
          v | v == t_Arrow ->
                case tas0 of
                 [] -> "(->)"
                 [t1] -> "( (->) " ++ niceNT m state al t1 ++ ")"
                 [t1,t2] -> '(':niceNT m state al t1 ++ " -> " ++ niceNT m state al t2++")"
          v | v == t_List  -> "[" ++ (case tas0 of [] -> ""; [t] -> niceNT m state al t) ++ "]"
          v -> '(': show (fixTid (mrpsIS state) v) ++ ' ': mixSpace (map (niceNT m state al) tas0) ++ ")"
niceNT m state al (NTcontext c a) =
        case (tidI . dropJust .  lookupIS state) c of
          TupleId _ -> '(' : niceNT m state al (mkNTvar a) ++ ")"
          v -> '(': show (fixTid (mrpsIS state) v) ++ ' ': niceNT m state al (mkNTvar a) ++ ")"


niceCtxs :: Eq a
         => Maybe ByteString
         -> IntState
         -> [(a,String)]
         -> [(Id,a)]
         -> String

niceCtxs _mmrps _state _al [] = ""
niceCtxs mmrps state al ctxs = "(" ++ mixComma (map ( \ (c,v) -> niceInt mmrps state c  (' ':assocDef al (error "niceCtx") v)) ctxs) ++ ") => "


niceInt :: Maybe ByteString -> IntState -> Id -> ShowS

niceInt Nothing state i     = (niceInfo (mrpsIS state) . dropJust . lookupIS state) i
niceInt (Just mrps) state i = (niceInfo  mrps          . dropJust . lookupIS state) i


niceTid :: IntState -> TokenId -> ShowS

niceTid state tid = (showsVar . fixTid (mrpsIS state)) tid


niceInfo :: ByteString -> Info -> ShowS

niceInfo mrps info = (showsVar . fixTid mrps . tidI) info


{- removes module name from qualified token if it is the current module name -}
fixTid :: ByteString -- name of current module
       -> TokenId
       -> TokenId

fixTid mrps (Qualified tid n) | tid == mrps = Visible n
fixTid _mrps v = v


{- produces renaming mapping for the free variables of given type -}
mkALNT :: NT -> [(Id,String)]

mkALNT = mkAL . freeNT


{- produces renaming mapping for given type ids -}
mkAL :: [a] -> [(a,String)]

mkAL tvs =
   let tvars = map (:[]) ['a'..'z'] ++ map (++"'") tvars
   in zip tvs tvars


niceNewType :: IntState -> NewType -> String

niceNewType state (NewType free _exist ctx nts) =
    let
      al = mkAL free ++ zip (map snd ctx) (map (('_':).(:[])) ['a'..'z']) -- a-z is too short!
    in niceCtxs Nothing state al ctx ++ mixSpace (map (niceNT Nothing state al) nts)


showsOp :: TokenId -> ShowS

showsOp tid =
        if isTidOp tid
        then shows tid
        else showChar '`' . shows tid . showChar '`'


showsVar :: TokenId -> ShowS

showsVar tid =
        if isTidOp tid
        then showChar '(' . shows tid . showChar ')'
        else shows tid


niceField :: IntState
          -> [(Id,String)]  -- renaming mapping for type variables
          -> (Maybe Id,NT)  -- (possible field id, its type)
          -> String

niceField state al (Just i,nt) = (showChar '{' . shows (fixTid (mrpsIS state) (tidIS state i)) . showString " :: ")
                                 (niceNT Nothing state al nt ++ "}")
niceField state al (Nothing,nt) =
   niceNT Nothing state al nt
