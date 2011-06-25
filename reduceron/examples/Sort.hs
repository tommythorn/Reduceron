module Sort where

data Nat = Z | S Nat

lte Z b = True
lte (S a) b =
  case b of
    Z -> False
    S b -> lte a b

insert a [] = [a]
insert a (b:bs) = if lte a b then a:b:bs else b:insert a bs

sort [] = []
sort (a:as) = insert a (sort as)

num :: Nat -> Int
num Z = 0
num (S z) = 1 + num z

nat :: Int -> Nat
nat n = if n == 0 then Z else S (nat (n-1))

downfrom :: Nat -> [Nat]
downfrom n = n : case n of
               Z -> []
               S a -> downfrom a

main :: Int
main = num (last (sort xs))
  where
    xs = downfrom (nat 500)
