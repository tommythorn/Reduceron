-- Division free Sieve
-- The result is sum [2,3,5,7,11,13,17,19,23,29] == 129
-- The Flite version is in Primes.fl
--
-- This will eventually run out of update stack if run for larger primes,
-- it's still unclear why and whether this is a bug.

module Main where
main = putStrLn $ show $ sum $ take 10 $ primes [2..]
primes (x:xs) = x : sieve x (primes xs)
sieve d l = mergeDiff l [d,d+d..]
mergeDiff xl@(x:xs) yl@(y:ys)
  | x > y  = mergeDiff xl ys
  | x < y  = x : mergeDiff xs yl
  | True   = mergeDiff xs ys
mergeDiff _ _ = []
