module Len where

fromTo :: Int -> Int -> [Int]
fromTo n m = if n == m then [] else n : fromTo (n+1) m

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

main :: Int
main = len (fromTo 1 1200)
