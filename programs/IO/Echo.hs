{- Simple IO exercise

Ideally,

main = do ch <- ld32 0
          st32 0 (ch + 1)
          main

but for now we can do this
-}

{
loop2 k ch = emit ch (loop k);
loop k = (ld32 0 13) (loop2 k);
main = loop 42;
}
