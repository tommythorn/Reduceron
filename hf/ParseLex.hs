module ParseLex where

import Lex
import Lexical
import Syntax(Lit(..),Boxed(..),Exp(..))
import ParseLib
import TokenId(isUnit,t_Bang,tprefix,tas,tunboxed,tprimitive,t_Tuple
              ,tforall,tdot,t_Arrow
              ,t_foreign,t_export,t_ccall,t_haskell,t_unsafe,t_cast,t_noproto
              ,t_stdcall,t_cplusplus,t_dotnet,t_jvm,t_safe
              ,tinterface,thiding,tqualified)


aanyid :: ParseGood (Exp TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
aanyop :: ParseGood (Exp TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
aconid :: ParseGood (Pos, TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
aconop :: ParseGood (Pos, TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
anyid ::  ParseGood (Exp TokenId) [(Pos, Lex, LexState, [PosTokenPre])] h
  -> ParseBad h [(Pos, Lex, LexState, [PosTokenPre])]
  -> [(Pos, Lex, LexState, [PosTokenPre])]
  -> ParseError
  -> ParseResult h [(Pos, Lex, LexState, [PosTokenPre])]
anyop ::  ParseGood (Exp TokenId) [(Pos, Lex, LexState, [PosTokenPre])] h
  -> ParseBad h [(Pos, Lex, LexState, [PosTokenPre])]
  -> [(Pos, Lex, LexState, [PosTokenPre])]
  -> ParseError
  -> ParseResult h [(Pos, Lex, LexState, [PosTokenPre])]
avarid :: ParseGood (Pos, TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
avarop :: ParseGood (Pos, TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
conid ::  ParseGood (Pos, TokenId) [(Pos, Lex, LexState, [PosTokenPre])] h
  -> ParseBad h [(Pos, Lex, LexState, [PosTokenPre])]
  -> [(Pos, Lex, LexState, [PosTokenPre])]
  -> ParseError
  -> ParseResult h [(Pos, Lex, LexState, [PosTokenPre])]
conop ::  ParseGood (Pos, TokenId) [(Pos, Lex, LexState, [PosTokenPre])] h
  -> ParseBad h [(Pos, Lex, LexState, [PosTokenPre])]
  -> [(Pos, Lex, LexState, [PosTokenPre])]
  -> ParseError
  -> ParseResult h [(Pos, Lex, LexState, [PosTokenPre])]
intPrim ::  ParseGood Int [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
k_as :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_cast :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_ccall ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_cplusplus ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_dot ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_dotnet :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_export :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_foreign ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_haskellcall ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_hiding :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_import :: ParseGood Pos [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
k_interface ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_jvm ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_noproto ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_prefix :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_primitive ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_qualified ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_rarrow :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_safe :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_stdcall ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_unboxed ::  ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_unit :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
k_unsafe :: ParseGood Pos [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
lit ::  Lex
  -> ParseGood Pos [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
tuple0 :: ParseGood (Pos, TokenId) [(Pos, Lex, e, f)] h
  -> ParseBad h [(Pos, Lex, e, f)]
  -> [(Pos, Lex, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, Lex, e, f)]
unboxed ::  ParseGood Bool [PosToken] c
  -> ParseBad c [PosToken]
  -> [PosToken]
  -> ParseError
  -> ParseResult c [PosToken]
varid ::  ParseGood (Pos, TokenId) [(Pos, Lex, LexState, [PosTokenPre])] h
  -> ParseBad h [(Pos, Lex, LexState, [PosTokenPre])]
  -> [(Pos, Lex, LexState, [PosTokenPre])]
  -> ParseError
  -> ParseResult h [(Pos, Lex, LexState, [PosTokenPre])]
varop ::  ParseGood (Pos, TokenId) [(Pos, Lex, LexState, [PosTokenPre])] h
  -> ParseBad h [(Pos, Lex, LexState, [PosTokenPre])]
  -> [(Pos, Lex, LexState, [PosTokenPre])]
  -> ParseError
  -> ParseResult h [(Pos, Lex, LexState, [PosTokenPre])]

lit a = literal (a::Lex)

eof :: Parser Pos [PosToken] c
eof = lit L_EOF

unboxed =
  True `parseChk` k_unboxed
  `orelse`
  parse False

lbrack :: Parser Pos [PosToken] c
lbrack = lit L_LBRACK
rbrack :: Parser Pos [PosToken] c
rbrack = lit L_RBRACK
lpar :: Parser Pos [PosToken] c
lpar = lit L_LPAR
rpar :: Parser Pos [PosToken] c
rpar = lit L_RPAR
lannot :: Parser Pos [PosToken] c
lannot = lit L_LANNOT
rannot :: Parser Pos [PosToken] c
rannot = lit L_RANNOT

notRannot :: Parser Pos [PosToken] c
notRannot = token (\pos t -> case t of L_RANNOT -> Left "/= #-}";  _x -> Right pos )

bang :: Parser Pos [PosToken] c
bang = lvarop t_Bang "!"

-- "special" identifiers which are *not* language keywords.
k_interface = lvarid tinterface "interface"
k_qualified = lvarid tqualified "qualified"
k_hiding = lvarid thiding "hiding"
k_as = lvarid tas "as"
k_unit = lconid (t_Tuple 0) "()"
k_primitive = lvarid tprimitive "primitive"
k_prefix = lvarid tprefix "prefix"
k_unboxed = lvarid tunboxed "unboxed"
k_forall ::
                ParseGood Pos [PosToken] c
                -> ParseBad c [PosToken]
                -> [PosToken]
                -> ParseError
                -> ParseResult c [PosToken]
k_forall = lvarid tforall "forall"
k_dot = lvarop tdot "dot"
k_rarrow = lvarop t_Arrow "->"

-- "special" identifiers for FFI which are not (all) language keywords.
k_foreign = lvarid t_foreign "foreign"
k_import = lit L_import
k_export = lvarid t_export "export"
k_ccall = lvarid t_ccall "ccall"
k_stdcall = lvarid t_stdcall "stdcall"
k_cplusplus = lvarid t_cplusplus "cplusplus"
k_dotnet = lvarid t_dotnet "dotnet"
k_jvm = lvarid t_jvm "jvm"
k_haskellcall = lvarid t_haskell "haskell"
k_safe = lvarid t_safe "safe"
k_unsafe = lvarid t_unsafe "unsafe"
k_noproto = lvarid t_noproto "noproto"
k_cast = lvarid t_cast "cast"

lvarop :: TokenId -> String -> Parser Pos [PosToken] c
lvarop tid str = token (\pos t -> case t of L_AVAROP v | v == tid -> Right pos;  _x -> Left str)

lvarid :: TokenId -> String -> Parser Pos [PosToken] c
lvarid tid str = token (\pos t -> case t of L_AVARID v | v == tid -> Right pos;  _x -> Left str)

lconid :: TokenId -> String -> Parser Pos [PosToken] c
lconid tid str = token (\pos t -> case t of L_ACONID v | v == tid -> Right pos;  _x -> Left str)

lcurl :: Parser Pos [PosToken] c
lcurl  = lit L_LCURL' `orelse` lit L_LCURL
larrow :: Parser Pos [PosToken] c
larrow = lit L_LessMinus
rarrow :: Parser Pos [PosToken] c
rarrow = lit L_MinusGreater
impl :: Parser Pos [PosToken] c
impl = lit L_EqualGreater
comma :: Parser Pos [PosToken] c
comma  = lit L_COMMA
semi :: Parser Pos [PosToken] c
semi = lit L_SEMI' `orelse` lit L_SEMI
equal :: Parser Pos [PosToken] c
equal  = lit L_Equal
pipe :: Parser Pos [PosToken] c
pipe = lit L_Pipe
dotdot :: Parser Pos [PosToken] c
dotdot = lit L_DotDot
coloncolon :: Parser Pos [PosToken] c
coloncolon = lit L_ColonColon
backtick :: Parser Pos [PosToken] c
backtick = lit L_BACKTICK

rational :: Parser (Pos,Lit Boxed) [PosToken] c
rational  = token (\pos t -> case t of L_RATIONAL x -> Right (pos, LitRational Boxed x) ; _ -> Left "<rational>")
integer :: Parser (Pos,Lit Boxed) [PosToken] c
integer = token (\pos t -> case t of L_INTEGER x -> Right (pos, LitInteger Boxed x) ; _ -> Left "<integer>")
int :: Parser (Pos,Lit Boxed) [PosToken] c
int = token (\pos t -> case t of L_INTEGER x -> Right (pos, LitInt Boxed (fromInteger x)) ; _ -> Left "<int>")
intPrim = token (\_pos t -> case t of L_INTEGER x -> Right ((fromInteger x) :: Int) ; _ -> Left "<intPrim>")

-- double :: Parser (Pos,Lit Boxed) [PosToken] c
-- double  = token (\pos t -> case t of L_DOUBLE x -> Right (pos, LitDouble Boxed x) ; _ -> Left "<double>")
char :: Parser (Pos,Lit Boxed) [PosToken] c
char   = token (\pos t -> case t of L_CHAR x   -> Right (pos, LitChar Boxed x) ; _ -> Left "<char>")
string :: Parser (Pos,Lit Boxed) [PosToken] c
string = token (\pos t -> case t of L_STRING x -> Right (pos, LitString Boxed x) ; _ -> Left "<string>")

tuple0 = token (\pos t -> case t of L_ACONID x | isUnit x -> Right (pos,x) ; _ -> Left "()")

aconid = token (\pos t -> case t of L_ACONID x -> Right (pos,x) ; _ -> Left "<conid>")
aconop = token (\pos t -> case t of L_ACONOP x -> Right (pos,x) ; _ -> Left "<conop>")
avarid = token (\pos t -> case t of L_AVARID x -> Right (pos,x)
--                                  L_primitive -> Right (pos,tprimitive)  -- Not a Haskell 1.3 reserved word
--                                  L_prefix   -> Right (pos,tprefix)  -- Not a Haskell 1.3 reserved word
--                                  L_unboxed  -> Right (pos,tunboxed) -- Not a Haskell 1.3 reserved word
--                                  L_as       -> Right (pos,tas)      -- Not a Haskell 1.3 reserved word
                                    _ -> Left "<varid>")
avarop = token (\pos t -> case t of L_AVAROP x -> Right (pos,x) ; _ -> Left "<varop>")

varid = avarid
           `orelse`
        lpar `revChk` avarop `chk` rpar
conid = aconid
           `orelse`
        lpar `revChk` aconop `chk` rpar

varop = avarop
           `orelse`
        backtick `revChk` avarid `chk` backtick


conop = aconop
           `orelse`
        backtick `revChk` aconid `chk` backtick

anyop = (uncurry ExpConOp) `parseAp` conop
           `orelse`
        (uncurry ExpVarOp) `parseAp`  varop

anyid = (uncurry ExpCon) `parseAp`  conid
           `orelse`
        (uncurry ExpVar) `parseAp`  varid

aanyid = (uncurry ExpCon) `parseAp` aconid
           `orelse`
         (uncurry ExpVar) `parseAp` avarid

aanyop = (uncurry ExpConOp) `parseAp` aconop
           `orelse`
         (uncurry ExpVarOp) `parseAp` avarop
