{- Simple IO exercise

Ideally,

main = do ch <- ld32 0
          st32 0 (ch + 1)
          main

but for now we can do this
-}

{
loop k = ld32 0 13 (\ch ->
         st32 0 '<' (
         st32 0 ch (
         st32 0 '>' (
         loop k))));
main = loop 42;
}
