{-# OPTIONS -fno-warn-name-shadowing #-}
module ParseLib(-- defined in ParseCore
                Pos, ParseError, ParseResult
               ,ParseBad, ParseGood, Parser
               ,initError,initBad,initGood      -- Start values for parseError,
                                                -- parseBad, parseGood
               ,parseit
               ,parse, ap, chk, orelse, into    -- The core
               ,token                           -- parse terminal
               ,parseFail                       -- Failing parser
                -- defined in ParseLib
               ,revAp                           -- Apply snd to fst
               ,revChk                          -- Check fst
               ,cases                           -- Muliple chk
               ,parseAp, parseChk               -- parse & (ap | chk)
               ,apCut, chkCut, intoCut          -- No return if fst succeed
               ,literal                         -- Parse literal
               ,optional, Maybe                 -- Zero or one item
               ,many, some                      -- Zero/one or more items.
                                                -- Cut after each item.
               ,manySep, someSep                -- Zero/one or more items with
                                                -- separator.  Cut after each
                                                -- item.
               ,manysSep, somesSep              -- Zero/one or more items with
                                                -- one or more separators. Cut
                                                -- after each item.
               ,rcurl                           -- Parse '}' and fix one if
                                                -- needed and possible.
               ,parseRest                       -- Always true, returns rest
                                                -- of the input
               ) where

import Lex
import Lexical(PosToken,lexicalCont)
import ParseCore

infixl 5 `parseAp`
infixl 5 `revAp`
infixl 5 `apCut`
infixl 5 `parseChk`
infixl 5 `revChk`
infixl 5 `chkCut`


revAp :: Parser a i c -> Parser (a->b) i c -> Parser b i c
revAp     x y = \good bad ->
                x       (\u -> y (\v -> let vu = v u in seq vu (good vu)) bad)
                        bad

revChk :: Parser a i c -> Parser b i c -> Parser b i c
revChk     x y = \good bad ->
                x       (\_  -> y good bad)
                        bad


cases :: [(Lex,Pos -> Parser  b  [PosToken] c)]
       -> Parser b  [PosToken] c
       -> Parser b  [PosToken] c
cases tps dp = \good bad input@((pos,t,_,_):input') err@(pe,_et,msg) ->
        if pe > pos then
                cases' pos t good input' (dp good bad input err) tps
        else
                cases'' pos t good input' (dp good bad input) pos (show t) (if pos > pe then [] else msg)  tps
        where

        cases' :: Pos -> Lex -> ParseGood b [PosToken] c
                                  -> [PosToken]
                                  -> ParseResult c [PosToken]
                                  -> [(Lex,Pos -> Parser b  [PosToken] c)]
                                  -> ParseResult c [PosToken]
        cases' _pos _t _good _input' dp [] =
                dp
        cases' pos t good input' dp ((t',p):tps) =
                if t == t' then
                        p pos good initBad input' initError
                else
                        cases' pos t good input' dp tps

        cases'' :: Pos -> Lex -> ParseGood b [PosToken] c
                                   -> [PosToken]
                                   -> (ParseError -> ParseResult c [PosToken])
                                   -> Pos
                                   -> String
                                   -> [String]
                                   -> [(Lex,Pos -> Parser b  [PosToken] c)]
                                   -> ParseResult c [PosToken]
        cases'' _pos _t _good _input' dp ep et em [] =
                dp (ep,et,em)
        cases'' pos t good input' dp ep et em ((t',p):tps) =
                if t == t' then
                        p pos good initBad input' initError
                else
                        cases'' pos t good input' dp ep et (show t' : em) tps


parseAp :: (a->b) -> Parser a i c -> Parser b i c
parseAp     x y = \good ->
                        y (\v -> let xv = x v in seq xv (good xv) )

parseChk :: b -> Parser a i c -> Parser b i c
parseChk    x y = \good ->
                        y (\_  -> good x)

apCut :: Parser (a->b) i c -> Parser a i c -> Parser b i c
apCut     x y = \good bad->
                x       (\u input' _err' -> y (\v -> let uv = u v in seq uv (good uv)) initBad input' initError)
                        bad

chkCut :: Parser b i c -> Parser a i c -> Parser b i c
chkCut     x y = \good bad ->
                x       (\u input' _err' -> y (\_ -> good u) initBad input' initError )
                        bad

intoCut :: Parser a i c -> (a->Parser b i c) -> Parser b i c
intoCut   x y = \good bad ->
                x       (\u input' _err' -> y u good initBad input' initError)
                        bad

---------  Next section doesn't care about the internal structure
literal ::
  (Eq b, Show b) =>
  b
  -> ParseGood Pos [(Pos, b, e, f)] h
  -> ParseBad h [(Pos, b, e, f)]
  -> [(Pos, b, e, f)]
  -> ParseError
  -> ParseResult h [(Pos, b, e, f)]
many :: Parser a i c -> Parser [a] i c
manySep ::
  Parser a i b
  -> Parser a1 i b
  -> ParseGood [a1] i b
  -> ParseBad b i
  -> i
  -> ParseError
  -> ParseResult b i
manySep' :: Parser a i b -> Parser a1 i b -> Parser [a1] i b
manysSep ::
  Parser a i c
  -> Parser a1 i c
  -> ParseGood [a1] i c
  -> ParseBad c i
  -> i
  -> ParseError
  -> ParseResult c i
manysSep' :: Parser a i c -> Parser a1 i c -> Parser [a1] i c
optional ::
  Parser a i b
  -> ParseGood (Maybe a) i b
  -> ParseBad b i
  -> i
  -> ParseError
  -> ParseResult b i
literal t = token (\pos t' -> if t==t' then Right pos else Left (show t))

optional p = Just `parseAp` p
                `orelse`
             parse Nothing

many p = some p `orelse` parse []

some :: Parser a i c -> Parser [a] i c
someSep ::
  (ParseGood a i b -> ParseBad b i -> i -> ParseError -> ParseResult b i)
  -> (ParseGood a1 i b -> ParseBad b i -> i -> ParseError -> ParseResult b i)
  -> Parser [a1] i b
somesSep ::
  (ParseGood a i c -> ParseBad c i -> i -> ParseError -> ParseResult c i)
  -> (ParseGood a1 i c -> ParseBad c i -> i -> ParseError -> ParseResult c i)
  -> Parser [a1] i c

some p = (:) `parseAp` p `apCut` many p

manySep' s p = s `revChk` someSep s p
                 `orelse`
               parse []

manySep s p = someSep s p `orelse` parse []
someSep s p = (:) `parseAp` p `apCut` manySep' s p

manysSep' s p = many s `revChk` somesSep s p
                  `orelse`
                parse []
manysSep s p = somesSep s p `orelse` parse []
somesSep s p = (:) `parseAp` p `apCut` manysSep' s p


--- Really specialized

rcurl :: Parser Lex  [PosToken] c
rcurl = \good bad (pt@(pos,t,_,_):input) err ->
        case t of
          L_RCURL  -> good t input err
          L_RCURL' -> good t input err
          _        -> case lexicalCont pt of
                        Left _m        -> bad (maxError (pos, show t ,["}'"]) err)
                        Right input'  -> good L_RCURL' input' err


-- Accept the rest of the input
parseRest :: Parser [PosToken]  [PosToken] c
parseRest = \good _bad input err -> good input input err
