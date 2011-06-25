module Primes where

mod2 :: Int -> Int -> Int
mod2 x y = if x == y then 0
                     else if x <= y then x
                                    else mod2 (x-y) y

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, y `mod2` x /= 0]

from :: Int -> [Int]
from n = n : from (n+1)

nthPrime :: Int -> Int
nthPrime n = sieve (from 2) !!! n

(!!!) :: [a] -> Int -> a
[] !!! n = [] !!! n
(x:xs) !!! n = if n == 0 then x else xs !!! (n-1)

main :: Int
main = nthPrime 350
