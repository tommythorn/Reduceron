definition module Flite;

import StdClass;
import StdInt;

:: List a = Nil | Cons a (List a);

:: Maybe a = Nothing | Just a;

:: Pair a b = Pair a b;

:: Ordering = LT | EQ | GT;

emit :: !Int a -> a;

emitInt :: !Int a -> a;

char :: !Char -> Int;

str :: !String -> List Int;

(/=) a b :== (<>) a b;
