module Rev where

rev [] acc = acc
rev (x:xs) acc = rev xs (x:acc)

total :: [Int] -> Int
total [] = 0
total (x:xs) = x + total xs

main = total (xs ++ xs ++ xs)
  where
    xs = rev ([0,1,2,3,4,5,6,7] :: [Int]) []
