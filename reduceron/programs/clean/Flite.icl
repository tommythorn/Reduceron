implementation module Flite;

import StdClass;
import StdFile;
import StdMisc;
import StdInt;
import StdChar;
import StdArray;
import StdString;
import StdOverloaded;
import StdDebug;

emit :: !Int a -> a;
emit c k = trace (toChar c) k;

emitInt :: !Int a -> a;
emitInt i k = trace i k;

char :: !Char -> Int;
char c = fromChar c;

str :: !String -> List Int;
str "" = Nil;
str s = Cons (char (select s 0)) (str (s % (1,size s)));
