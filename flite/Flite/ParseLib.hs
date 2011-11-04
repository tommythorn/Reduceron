module Flite.ParseLib where

import Data.Char

infixr 3 <|>
infixl 4 <*>
infixl 5 <|
infixl 6 |>

type Parser a = String -> [(String, a)]

pure :: a -> Parser a
pure a = \s -> [(s, a)]

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
f <*> a = \s -> [(s1, g b) | (s0, g) <- f s, (s1, b) <- a s0]

(|>) :: Parser a -> Parser b -> Parser b
a |> b = pure (\a b -> b) <*> a <*> b

(<|) :: Parser a -> Parser b -> Parser a
a <| b = pure (\a b -> a) <*> a <*> b

(<|>) :: Parser a -> Parser a -> Parser a
a <|> b = \s -> take 1 (a s ++ b s)

guarded :: (a -> Bool) -> Parser a -> Parser a
guarded f p = \s -> [(s', a) | (s', a) <- p s, f a]

sat :: (Char -> Bool) -> Parser Char
sat f "" = []
sat f (c:s) = [(s, c) | f c]

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string "" = pure ""
string (c:cs) = pure (:) <*> char c <*> string cs

alphanum :: Parser Char
alphanum = sat isAlphaNum

digit :: Parser Int
digit = pure (\c -> ord c - ord '0') <*> sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = pure (:) <*> p <*> many p

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = p <| space

tok :: String -> Parser String
tok = token . string

nat :: Parser Int
nat = token natural

natural :: Parser Int
natural = pure total <*> many1 digit
  where total = foldl (\acc n -> 10*acc + n) 0

int :: Parser Int
int = token integer

integer :: Parser Int
integer = natural <|> pure negate <*> (char '-' |> natural)

strLit :: Parser String
strLit s@('"':_) = map swap (lex s)
  where swap (a, b) = (b, a)
strLit _ = []

charLit :: Parser String
charLit s@('\'':_) = map swap (lex s)
  where swap (a, b) = (b, a)
charLit _ = []

parse :: Parser a -> String -> a
parse p s =
  case p s of
    []        -> error "Parse error"
    [("", x)] -> x
    [(s, x)]  -> error "Parse error"
    _         -> error "Ambiguous parse --- this shouldn't happen!"
