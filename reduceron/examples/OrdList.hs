module OrdList where

data Nat = Z | S Nat

False ==> _ = True
True ==> a = a

ord [] = True
ord (x:xs) =
  case xs of
    [] -> True
    y:ys -> (x ==> y) && ord xs

insert x [] = [x]
insert x (y:ys) = if x ==> y then x:y:ys else y:insert x ys

prop x xs = ord xs ==> ord (insert x xs)

boolList Z = [[]]
boolList (S n) =
  boolList n
    ++ map (False:) (boolList n)
    ++ map (True:) (boolList n)

main :: Int
main = if and [prop x xs | x <- [False, True], xs <- boolList eleven]
       then 1 else 0
  where
    eleven = S (S (S (S (S (S (S (S (S (S (S Z))))))))))
