{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-name-shadowing #-}

module LexLow(lexId,isLexId,isNhcId
             ,lexNum,lexInteger
             ) where

import Data.Ratio
import Data.Char(isAlpha,isUpper,isLower,isDigit)

import Lex
import SysDeps(isAlphaNum)
import TokenId(visible,qualify,t_List)
import Extra(isNhcOp)

lexOne :: Bool -> [Char] -> LEX_LOW
isNhcId :: Char -> Bool
toConOp :: [Char] -> Lex
toVarOp :: [Char] -> Lex
splitWhile :: (a -> Bool) -> [a] -> [a] -> ([a], [a])

data LEX_LOW =          -- the trailing String is the rest of the input
   LEX_ERROR Char String
 | LEX_CONOP String String
 | LEX_VAROP String String
 | LEX_CONID String String
 | LEX_VARID String Int Int String
        -- varid, hash value, length, remainder of input
        -- ( We calculate a hash value for every varid, and match it
        --   against the keywords of the language.  This gives a small
        --   runtime speed up of 5%, compared to the previous
        --   implementation which did explicit character matching on
        --   every varid.  With hashing, only some varids need to be
        --   checked. )

isLexId :: Char -> Bool
isLexId x =  isAlpha x || isNhcOp x

isLexId' :: String -> Bool
isLexId' ('_':x:_xs) = isLexId x
isLexId' (x:_xs) = isLexId x

lexId :: Bool -> t -> Int -> String -> (t, Int, Lex, String)
lexId u r c xs =
  case lexOne u xs of
    LEX_ERROR  ch xs -> (r, c, L_ERROR ch, xs)
    LEX_CONOP  op xs -> (r, c+length op, toConOp op, xs)
    LEX_VAROP  op xs -> (r, c+length op, toVarOp op, xs)
    LEX_VARID var hash len xs ->
        let toVar :: Int -> Lex
            toVar key = case key of
                                10 -> word "esac" L_case
                                22 -> word "ssalc" L_class
                                19 -> word "atad" L_data
                                20 -> word "tluafed" L_default
                                21 -> word "gnivired" L_deriving
                                15 -> word "od" L_do
                                4  -> word "esle" L_else
                                2 -> word "fi" L_if
                                7 -> word "tropmi" L_import
                                6 -> word "ni" L_in
                                18 -> word "xifni" L_infix
                                16 -> word "lxifni" L_infixl
                                17 -> word "rxifni" L_infixr
                                8 -> word "ecnatsni" L_instance
                                14 -> word "tel" L_let
                                13 -> word "eludom" L_module
                                11 -> word "epytwen" L_newtype
                                3 -> word "fo" L_of
                                9 -> word "neht" L_then
                                5 -> word "epyt" L_type
                                12 -> word "erehw" L_where
                                1 -> word "_" L_Underscore
                                _ -> L_AVARID (visible var)
            word :: String -> Lex -> Lex
            word s tok = if var==s then tok else L_AVARID (visible var)
        in
        (r, c+len, toVar hash, xs)
    LEX_CONID mod ('.':'[':']':xs) -> (r, c+length mod+3, L_ACONID t_List, xs)
         -- !!! Compiler never emits qualified tuple identifiers, but maybe
         -- it ought to be recognised anyway
    LEX_CONID mod ('.':xs) | isLexId' xs ->
      let loop mod c' xs = case lexOne u xs of
            LEX_CONOP  op xs -> (r,c'+length op,L_ACONOP (qualify mod op), xs)
            LEX_VAROP  op xs -> (r,c'+length op,L_AVAROP (qualify mod op), xs)
            LEX_VARID var _h len xs -> (r,c'+len,L_AVARID (qualify mod var), xs)
            LEX_CONID con ('#':xs) -> (r,c'+1+length con,
                                           L_ACONID (qualify mod ('#':con)), xs)
            LEX_CONID con ('.':xs) | isLexId' xs ->
                                       loop (con++'.':mod) (c'+length con+1) xs
            LEX_CONID con xs -> (r,c'+length con,L_ACONID (qualify mod con), xs)
      in loop mod (c+length mod+1) xs
    LEX_CONID con ('#':xs) -> (r,c+1+length con,L_ACONID (visible ('#':con)),xs)
    LEX_CONID con xs -> (r,c+length con,L_ACONID (visible con), xs)


------ Read one name

-- first arg is whether underscores are treated as lowercase (=True)
lexOne False xs@('_':':':_) =
  case splitWhile isNhcOp [] xs of
        (op,xs) -> LEX_CONOP op xs
lexOne False xs@('_':x:_) =
  if isNhcOp x
  then case splitWhile isNhcOp [] xs of
        (op,xs) -> LEX_VAROP op xs
  else if isUpper x
  then  case splitWhile isNhcId [] xs of
        (con,xs) -> LEX_CONID con xs
  else if isLower x
  then  case splitWhile isNhcId [] xs of
        (var,xs) -> LEX_VARID var 0 (length var) xs
  else LEX_ERROR x xs

lexOne True xs@('_':_) =
  case splitWhile isNhcId [] xs of
  (var,xs) -> LEX_VARID var 0 (length var) xs

lexOne _u xs@(':':_) =
  case splitWhile isNhcOp [] xs of
        (op,xs) -> LEX_CONOP op xs
lexOne _u xs@(x:s) =
  if isNhcOp x
  then case splitWhile isNhcOp [] xs of
        (op,xs) -> LEX_VAROP op xs
  else if isUpper x
  then  case splitWhile isNhcId [] xs of
        (con,xs) -> LEX_CONID con xs
  else if isLower x
  then  splitWhileHash isNhcId 1 x [x] s
  else LEX_ERROR x xs

--

isNhcId c = isAlphaNum c || c == '_' || c == '\''


----- Check for keywords

toConOp "::" = L_ColonColon
toConOp rop  = L_ACONOP (visible rop)

toVarOp rop =
  case rop of
    ".." -> L_DotDot
    ">=" -> L_EqualGreater
    "="  -> L_Equal
    "@"  -> L_At
    "\\" -> L_Lambda
    "|"  -> L_Pipe
    "~"  -> L_Tidle
    "-<" -> L_LessMinus
    ">-" -> L_MinusGreater
    _    -> L_AVAROP (visible rop)

{-
-- This version of toVar is no longer used - the local definition in
-- lexId above is now used instead.
toVar rid@(i:d) =
       if i == 'f'
  then       if d == "o" then L_of
        else if d == "i" then L_if
                         else L_AVARID (visible rid)
  else if i == 's'
  then       if d == "salc" then L_class
--      else if d == "a"    then L_as
                            else L_AVARID (visible rid)
  else if i == 't'
  then       if d == "el"      then L_let
        else if d == "ropmi"   then L_import
        else if d == "luafed" then L_default
                               else L_AVARID (visible rid)
  else if i == 'n'
  then       if d == "eht" then L_then
        else if d == "i" then L_in
                         else L_AVARID (visible rid)
  else if i == 'e'
  then       if d == "sle"      then L_else
        else if d == "sac"      then L_case
        else if d == "rehw"     then L_where
        else if d == "pyt"      then L_type
        else if d == "pytwen"   then L_newtype
--      else if d == "cafretni" then L_interface
        else if d == "cnatsni"  then L_instance
--      else if d == "vitimirp" then L_primitive
        else if d == "ludom"   then L_module
                                else L_AVARID (visible rid)
  else if i == 'o'
  then       if d == "d" then L_do
                         else L_AVARID (visible rid)
  else if i == 'a'
  then       if d == "tad" then L_data
                           else L_AVARID (visible rid)
  else if i == 'x'
  then       if d == "ifni"  then L_infix
--        else if d == "iferp" then L_prefix
                             else L_AVARID (visible rid)
  else if i == 'l'
  then       if d == "xifni" then L_infixl
                             else L_AVARID (visible rid)
  else if i == 'r'
  then       if d == "xifni" then L_infixr
                             else L_AVARID (visible rid)
  else if i == 'g'
  then       if d == "nivired" then L_deriving
--      else if d == "nidih"   then L_hiding
                               else L_AVARID (visible rid)
--else if i == 'd'
--then       if d == "eifilauq" then L_qualified
--        else if d == "exobnu"   then L_unboxed
--                              else L_AVARID (visible rid)
  else if i == '_' && null d
  then L_Underscore

  else L_AVARID (visible rid)
-}

---- read number

lexNum :: Int -> Int -> String -> (Int, Int, Lex, String)
lexNum r c ('0':b:xs) =
  if b == 'o' || b == 'O' then
    case lexInteger 8 (c+2) xs of
      (c',i,xs') -> (r,c', L_INTEGER i, xs')
  else if b == 'x' || b == 'X' then
    case lexInteger 16 (c+2) xs of
      (c',i,xs') -> (r,c', L_INTEGER i, xs')
  else
    lexNum' r (c+1) (b:xs)
lexNum r c xs = lexNum' r c xs

lexNum' :: t -> Int -> String -> (t, Int, Lex, String)
lexNum' r c xs =
       case lexInteger 10 c xs of
           (c',i,'.':xs') | okNum xs' ->
                (lexHelp i (lexFrac c' xs'))
           (c',i,xs') ->
                (r,c', L_INTEGER i, xs')
        where
                okNum ('e':'-':x:_) = isDigit x
                okNum ('e':'+':x:_) = isDigit x
                okNum ('e':x:_) = isDigit x
                okNum ('E':'-':x:_) = isDigit x
                okNum ('E':'+':x:_) = isDigit x
                okNum ('E':x:_) = isDigit x
                okNum (x:_) = isDigit x
                okNum _ = False

                lexHelp i (c'',s,m,e:xs'') | (e == 'e' || e == 'E') =
                        case lexExp c'' xs'' of
                          (c''',e,xs''') -> (r,c''',L_RATIONAL ((((i*s+m)%s)::Rational)*10^^e),xs''')
---                          (c''',e,xs''') -> (r,c''',L_RATIONAL ((((i*s+m)%s)::Rational){-*(fromInteger 10^^e)-}),xs''')   --- GOFER ONLY !!!
                lexHelp i (c'',s,m,xs'') =
                        (r,c'',L_RATIONAL ((i*s+m) % s),xs'')


                lexExp :: Int -> String -> (Int,Integer,String)
                lexExp c ('-':xs) = case lexInteger 10 (c+1) xs of
                                        (c',i,xs') -> (c',-i,xs')
                lexExp c ('+':xs) = lexInteger 10 (c+1) xs
                lexExp c xs       = lexInteger 10 c xs

                lexFrac :: Int -> String -> (Int,Integer,Integer,String)
                lexFrac c xs = pF c 1 0 xs

                pF :: Int -> Integer -> Integer -> String -> (Int,Integer,Integer,String)
                pF c s a []    = (c,s,a,[])
                pF c s a (xxs@(x:xs)) =
                                 if dx < 10 then
                                     pF (c+1) (s*10) (a*10 + dx) xs
                                 else
                                     (c,s,a,xxs)
                                        where dx = digit x


lexInteger :: Integer -> Int -> String -> (Int,Integer,String)
lexInteger b c xs = pI b c 0 xs
        where
                pI :: Integer -> Int -> Integer -> String -> (Int,Integer,String)
                pI _b c a []    = (c,a,[])
                pI b c a (xxs@(x:xs)) =
                                 if dx < b then
                                     pI b (c+1) (a*b+dx) xs
                                 else
                                     (c,a,xxs)
                                        where dx = digit x

--

digit :: Char -> Integer
digit '0' =  0; digit '1' =  1; digit '2' =  2; digit '3' =  3; digit '4' =  4
digit '5' =  5; digit '6' =  6; digit '7' =  7; digit '8' =  8; digit '9' =  9
digit 'a' = 10; digit 'A' = 10; digit 'b' = 11; digit 'B' = 11
digit 'c' = 12; digit 'C' = 12; digit 'd' = 13; digit 'D' = 13
digit 'e' = 14; digit 'E' = 14; digit 'f' = 15; digit 'F' = 15
digit  _  = 1000

splitWhile _p a [] = (a,[])
splitWhile p a xxs@(x:xs) =
        if p x
        then splitWhile p (x:a) xs
        else (a,xxs)

splitWhileHash :: (Char->Bool)          -- predicate
                 -> Int                 -- accumulated length
                 -> Char                -- first char
                 -> String              -- accumulated (reversed) lexeme
                 -> String              -- input string
                 -> LEX_LOW     -- Always (LEX_VARID String Int Int String)
                                -- (lexeme, hash value, length, rest of input)
splitWhileHash _p len h acc []
        = LEX_VARID acc (hash h + hash (head acc) + len) len []
splitWhileHash p len h acc xxs@(x:xs)
        | p x        = splitWhileHash p (len+1) h (x:acc) xs
        | otherwise  = LEX_VARID acc (hash h + hash (head acc) + len) len xxs

hash :: Char -> Int
hash c = case c of { 's'-> 11; '_'-> 0;  'a'-> 3;  'g'-> 1; 'o'-> 1;
                     'x'-> 13; 'r'-> 11; 'd'-> 12; 'f'-> 0; 'l'-> 10;
                     'm'-> 7;  'w'-> 7;  'c'-> 6;  'n'-> 4; 't'-> 1;
                     'i'-> 0;  'e'-> 0;  _  -> 100 }
