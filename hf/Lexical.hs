{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-deprecations #-}
{-
Lexical analysis of a file.
-}

module Lexical(lexical,lexicalCont,Lex
                ,LexState,PosToken,PosTokenPre,Pos) where

import Extra(Pos,toPos,strPos,insertPos)
import Lex
import LexPre
import SysDeps(PackedString,packString,unpackPS)
import TokenId

type PosToken = (Pos,Lex, LexState, [PosTokenPre])
type LexState = [Int]  -- stack of indentations of {} blocks

-- 0 : no active indentation (explicit layout)

lexical :: Bool -> [Char] -> [Char] -> [PosToken]
 -- with H'98 underscore -> filename -> file content -> tokens
-- lexPre basically does the lexing, but afterwards iLex handles
-- indentation for the layout rule
lexical u file l = iLex [0] 0 (beginning (lexPre u file' l))
  where
    file' = packString file
    -- handle pragmas and start and missing "module" header
    beginning :: [PosTokenPre] -> [PosTokenPre]
    beginning toks =
       case toks of
           lp@((_f,_r,_c,L_module):_)    ->  lp
           lp@((_f,_r,_c,L_AVARID t):_) | t==tinterface ->  lp
           (lp@(_f,_r,_c,L_LANNOT):rest) ->  lp: discard_pragma rest
           lp                         ->  ((file',toPos 1 0 0 0,0,L_module)
                                          :(file',toPos 1 0 0 0,0,L_ACONID tMain)
                                          :(file',toPos 1 0 0 0,0,L_where)
                                          :lp)
    discard_pragma (lp@(_f,_r,_c,L_RANNOT):rest) = lp: beginning rest
    discard_pragma (lp@(_f,_r,_c,_):rest)        = lp: discard_pragma rest

lexicalCont :: PosToken -> Either String [PosToken]
lexicalCont (p,t,(i:s@(i':_)),r) =
                if i > 0
                then -- Right ((p,t,s,r) : iLex s i' r) -- not correct?
                     case r of
                       ((f,_,_,_):_) -> Right (piLex f s i' p t r)
                else Left "Layout }"
lexicalCont (_p,_t, []  ,_r) = 
                Left "Layout }"

---  local

iLex :: LexState -> Int -> [PosTokenPre] -> [PosToken]
iLex _s _i [] = []
iLex s i ((f,p,c,t):pt) = 
  seq p $
  if c > i then
    piLex f s i p t pt
  else if c == i && i /= 0 && t /= L_in then
    seq p' $ (p',L_SEMI',s,pt) : piLex f s i p t pt
  else if c == 0 && i == 0 then
    piLex f s i p t pt
  else
    seq p' $ (p',L_RCURL',s,pt) : iLex s' i' ((f,p,c,t):pt)
  where
  (_:s'@(i':_)) = s
  p' = insertPos p

piLex :: PackedString -> LexState -> Int -> Pos -> Lex -> [PosTokenPre] 
      -> [PosToken]
piLex _file s i p tok tr@((f,p',c,t'):pt)
      | tok `elem` [L_let, L_where, L_of, L_do] =
          (p,tok,s,tr)
          : if t' == L_LCURL 
              then seq p' $ (p',L_LCURL, s,pt) : iLex (0:s) 0 pt 
            else
                let p'' = insertPos p' in seq p'' $ (p'', L_LCURL',s,tr)
                : if c > i then
                    seq p' $ piLex f (c:s) c p' t' pt
                  else
                    (p, L_RCURL',s,tr) : iLex s i tr
piLex _file s _i p L_LCURL  pt =
          (p,L_LCURL,s,pt)
          : iLex (0:s) 0 pt
piLex file s i p L_RCURL  pt = 
      if i == 0
      then case s of 
             (_:s'@(i':_)) -> (p,L_RCURL,s,pt) : iLex s' i' pt
             _             -> failPos file p "Unbalanced '}' (Stack empty)."
      else failPos file p "Unbalanced '}' (No explicit '{' in scope)"
piLex _file s i p t pt  =
          (p,t,s,pt)
          : iLex s i pt


failPos :: PackedString -> Pos -> [Char] -> a
failPos file p msg = error ("Internal in " ++ unpackPS file ++ " at " ++ strPos p ++ ": " ++ msg ++ "\n")
