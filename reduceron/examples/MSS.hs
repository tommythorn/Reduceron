module MSS where

undef = undef

myinit xs =
  case xs of
    [] -> undef
    x:xs -> case xs of
              [] -> []
              y:ys -> x : myinit xs

inits xs = case xs of
             [] -> [xs]
             y:ys -> xs : inits (myinit xs)

tails [] = []
tails xxs@(x:xs) = xxs : tails xs

conc [] = []
conc (x:xs) = x ++ conc xs

segments xs = conc (map tails (inits xs))

maxi :: [Int] -> Int
maxi xs =
  case xs of
    [] -> undef
    x:xs -> case xs of
              [] -> x
              y:ys -> if x <= y then maxi xs else maxi (x:ys)

total :: [Int] -> Int
total [] = 0
total (x:xs) = x + total xs

mss :: [Int] -> Int
mss xs = maxi (map total (segments xs))

fromTo :: Int -> Int -> [Int]
fromTo n m = if n <= m then n : fromTo (n+1) m else []

main :: Int
main = mss (fromTo (0-150) 150)
