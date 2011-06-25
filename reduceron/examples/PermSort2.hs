module PermSort where

undef = undef

hd [] = undef
hd (x:xs) = x

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations (x:xs) = concatMap (place x) (permutations xs)
  where place x [] = [[x]]
        place x (y:ys) = (x:y:ys) : map (y:) (place x ys)

ord :: [Int] -> Bool
ord [] = True
ord [x] = True
ord (x:y:ys) = x <= y && ord (y:ys)

filt p [] = []
filt p (x:xs) = if p x then x : filt p xs else filt p xs

permSort xs = hd (filt ord (permutations xs))

main :: Int
main = hd (permSort (reverse (fromTo 1 9)))

fromTo :: Int -> Int -> [Int]
fromTo n m = n : if n == m then [] else fromTo (n+1) m
