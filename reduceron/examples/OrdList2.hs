module OrdList2 where

data Nat = Z | S Nat

ord [] = True
ord (x:xs) =
  case xs of
    [] -> True
    y:ys -> if x then (if y then ord xs else False) else ord xs

insert x [] = [x]
insert x (y:ys) =
  if x then (if y then x:y:ys else y : insert x ys) else x:y:ys

prop x xs = if ord xs then ord (insert x xs) else True

place x [] = []
place x (y:ys) = (x:y) : place x ys

boolList Z = [[]]
boolList (S n) =
  boolList n
    ++ place False (boolList n)
    ++ place True (boolList n)

andList [] = False
andList (x:xs) = if x then andList xs else False

main :: Int
main = if andList [prop x xs | x <- [False, True], xs <- boolList eleven]
       then 1 else 0
  where
    eleven = S (S (S (S (S (S (S (S (S (S (S Z))))))))))
