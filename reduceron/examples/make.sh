#!/bin/sh

yhc --linkcore Fib.hs
yhc --linkcore Exp.hs
yhc --linkcore Queens.hs
yhc --linkcore Flipper.hs
yhc --linkcore Len.hs
yhc --linkcore SumPuz.hs
yhc --linkcore Primes.hs
yhc --linkcore PermSort.hs
yhc --linkcore Adjoxo.hs
yhc --linkcore Sudoku.hs
yhc --linkcore Simplifier.hs
yhc --linkcore OrdList.hs
yhc --linkcore Sem.hs
yhc --linkcore Backtrack.hs
yhc --linkcore Divisors.hs

./clean.sh
