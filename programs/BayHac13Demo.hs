{

fib n = case (<=) n 1 of {
          True  -> 1;
          False -> (+) (fib ((-) n 2)) (fib ((-) n 1));
        };

seq n = case (<=) n 35 of {
          True -> st32 0 (fib n) (seq ((+) n 1));
          False -> seq 0;
        };

main = seq 0;
-- main = st32 0 240 7;
}
