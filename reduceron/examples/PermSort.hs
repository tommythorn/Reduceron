module PermSort where

undef = undef

hd [] = undef
hd (x:xs) = x

insert x [] sk fk = sk [x] fk
insert x (y:ys) sk fk =
  sk (x:y:ys) (insert x ys (\x fk -> sk (y:x) fk) fk)

permute [] sk fk = sk [] fk
permute (x:xs) sk fk = permute xs (\y fk -> insert x y sk fk) fk

ord :: [Int] -> Bool
ord [] = True
ord [x] = True
ord (x:y:ys) = x <= y && ord (y:ys)

filt p [] = []
filt p (x:xs) = if p x then x : filt p xs else filt p xs

permSort xs = hd (filt ord (permute xs (:) []))

main :: Int
main = hd (permSort (reverse (fromTo 1 9)))

fromTo :: Int -> Int -> [Int]
fromTo n m = n : if n == m then [] else fromTo (n+1) m
