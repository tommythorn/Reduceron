module Main where {

import Prelude();
import Flite;
default(Int);

fib n = case (<=) n 1 of {
          True  -> 1;
          False -> (+) (fib ((-) n 2)) (fib ((-) n 1));
        };

main = print (fib 36);

}
