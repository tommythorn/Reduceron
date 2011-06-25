module Flipper where

main :: Int
main = case x && x of
         True -> 1
         False -> 0
  where
    x = notn 200001 False

notn :: Int -> Bool -> Bool
notn 0 x = x
notn n x = not (notn (n-1) x)
