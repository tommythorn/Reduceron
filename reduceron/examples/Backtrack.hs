module Backtrack where

insert x [] sk fk = sk [x] fk
insert x (y:ys) sk fk =
  sk (x:y:ys) (insert x ys (\x fk -> sk (y:x) fk) fk)

permute [] sk fk = sk [] fk
permute (x:xs) sk fk = permute xs (\y fk -> insert x y sk fk) fk

total :: [Int] -> Int
total [] = 0
total (x:xs) = x + total xs

main :: Int
main = total $ last $ permute xs (:) []
  where
    xs :: [Int]
    xs = [1,2,3,4,5,6,7,8,9,10]
