module Fib where

fib   :: Int -> Int
fib n =  if n <= 1 then 1 else fib (n-2) + fib (n-1)

main :: Int
main =  fib 31

{- Also works, but larger space usage

fib   :: Int -> Int
fib 0 =  1
fib 1 =  1
fib n =  fib (n-2) + fib (n-1)

main :: Int
main =  fib 10

-}
