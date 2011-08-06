module Flite 
  ( (<=)
  , (+)
  , (-)
  , (==)
  , (/=)
  , emit
  , emitInt
  , P.Int
  , P.print
  , P.Bool(..)
  , List(..)
  , Maybe(..)
  , Pair(..)
  , Ordering(..)
  , char
  , str
  , mainFunc
  ) where

import qualified Prelude as P
import System.IO.Unsafe(unsafePerformIO)

(<=) :: P.Int -> P.Int -> P.Bool
a <= b = a P.<= b

(+) :: P.Int -> P.Int -> P.Int
a + b = a P.+ b

(-) :: P.Int -> P.Int -> P.Int
a - b = a P.- b

(==) :: P.Int -> P.Int -> P.Bool
a == b = a P.== b

(/=) :: P.Int -> P.Int -> P.Bool
a /= b = a P./= b

emit :: P.Int -> a -> a;
emit i k = unsafePerformIO (do { P.putChar (P.toEnum i) ; P.return k });

emitInt :: P.Int -> a -> a;
emitInt i k = unsafePerformIO (do { P.putStr (P.show i) ; P.return k });

data List a = Nil | Cons a (List a) deriving P.Show

data Maybe a = Nothing | Just a deriving P.Show

data Pair a b = Pair a b deriving P.Show

data Ordering = LT | EQ | GT deriving P.Show

char :: P.Char -> P.Int
char = P.fromEnum

str :: P.String -> List P.Int
str xs
  | P.null xs = Nil
  | P.otherwise = Cons (char (P.head xs)) (str (P.tail xs))

mainFunc :: P.Int -> P.IO ()
mainFunc x = P.seq x (P.return ())
