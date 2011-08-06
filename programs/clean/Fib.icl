module Fib;

import StdClass;
import StdInt;

fib n = case (<=) n 1 of {
          True  -> 1;
          False -> (+) (fib ((-) n 2)) (fib ((-) n 1));
        };

Start = fib 36;
