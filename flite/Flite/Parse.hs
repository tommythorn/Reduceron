module Flite.Parse where

import Flite.ParseLib
import Flite.Syntax
import Data.Char

parseProg = parse prog

keywords :: [String]
keywords =
  [ "case", "of", "let"
  , "in", "emit", "emitInt"
  , "if", "then", "else"
  ]

identifier :: Parser Char -> Parser String
identifier begin = token (guarded g (pure (:) <*> begin <*> many alphanum))
  where g s = s `notElem` keywords

lowerIdent :: Parser String
lowerIdent = identifier lower

upperIdent :: Parser String
upperIdent = identifier upper

key :: String -> Parser String
key s = token $ \input ->
  [(rest, a) | (rest, a) <- string s input
             , null rest || not (isAlphaNum (head rest))]

prog :: Parser Prog
prog = block defn

block :: Parser a -> Parser [a]
block p = tok "{" |> seq <| tok "}"
  where seq = seq' <| (tok ";" <|> pure "")
        seq' = pure (:) <*> p <*> many (tok ";" |> p)

defn :: Parser Decl
defn = pure Func <*> lowerIdent <*> many pat <*> tok "=" |> expr

expr :: Parser Exp
expr = pure App <*> expr' <*> many expr'

expr' :: Parser Exp
expr' = pure Case <*> (key "case" |> expr) <*> (key "of" |> block alt)
    <|> pure Let  <*> (key "let" |> block bind) <*> (key "in" |> expr)
    <|> pure Var  <*> lowerIdent
    <|> pure Con  <*> upperIdent
    <|> pure Int  <*> nat
    <|> pure Fun  <*> prim
    <|> ifte
    <|> pure charList <*> token strLit
    <|> pure oneChar <*> token charLit
    <|> tok "(" |> expr <| tok ")"

prim :: Parser String
prim = tok "(+)" <|> tok "(-)" <|> tok "(==)" <|> tok "(/=)" <|> tok "(<=)"
   <|> key "emit" <|> key "emitInt"

pat :: Parser Pat
pat = pure Var <*> lowerIdent
  <|> pure (\s -> App (Con s) []) <*> upperIdent
  <|> tok "(" |> pat' <| tok ")"

pat' :: Parser Pat
pat' = pure Var <*> lowerIdent
   <|> pure App <*> (pure Con <*> upperIdent) <*> many pat

bind :: Parser Binding
bind = pure (,) <*> (lowerIdent <| tok "=") <*> expr

alt :: Parser Alt
alt = pure (,) <*> (pat' <| tok "->" ) <*> expr

ifte :: Parser Exp
ifte = pure cond <*> (key "if" |> expr)
                 <*> (key "then" |> expr)
                 <*> (key "else" |> expr)
  where
    cond e1 e2 e3 = Case e1 [ (App (Con "True") [], e2)
                            , (App (Con "False") [], e3)
                            ]

charList :: String -> Exp
charList s = charList' (read s)
  where
    charList' "" = Con "Nil"
    charList' (c:cs) = App (Con "Cons") [Int (fromEnum c), charList' cs]

oneChar :: String -> Exp
oneChar s = Int (fromEnum (read s :: Char))
