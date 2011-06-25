module Exp where

infix 8 ^^^

data Nat = Z | S Nat

add Z y = y
add (S x) y = S (add x y)

mul x Z = Z
mul x (S y) = add x (mul x y)

nat   :: Int -> Nat
nat x =  if x <= 0 then Z else S (nat (x-1))

int :: Nat -> Int
int Z     = 0
int (S x) = 1 + int x

x ^^^ Z   = S Z
x ^^^ S y = mul x (x ^^^ y)

main = int (nat 3 ^^^ nat 6) + int (nat 3 ^^^ nat 6)
     + int (nat 3 ^^^ nat 6) + int (nat 3 ^^^ nat 6)
     + int (nat 3 ^^^ nat 6) + int (nat 3 ^^^ nat 6)
     + int (nat 3 ^^^ nat 6) + int (nat 3 ^^^ nat 6)

{- Also works, but larger space usage

infix 8 ^^^

data Nat = Z | S Nat deriving (Eq,Show)

instance Num Nat where
    Z   + y   = y
    S x + y   = S (x + y)
    x   * Z   = Z
    x   * S y = x * y + x

nat   :: Int -> Nat
nat x =  if x <= 0 then Z else S (nat (x-1))

int :: Nat -> Int
int Z     = 0
int (S x) = 1 + int x

x ^^^ Z   = S Z
x ^^^ S y = x * (x ^^^ y)

main = int (nat 2 ^^^ nat 5)

-}
