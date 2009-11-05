{

fib n = if (<=) n 1 then 1 else (+) (fib ((-) n 2)) (fib ((-) n 1));

emitStr Nil k = k;
emitStr (Cons x xs) k = emit x (emitStr xs k);

main = emitStr "fib(10) = " (emitInt (fib 10) (emit '\n' 0));

}
