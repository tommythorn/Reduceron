module Divisors where

False <#> x = x
True <#> x = case x of { False -> True ; True -> False }

halfAdd a b = (a <#> b, a && b)

fullAdd carryIn a b = (carryIn <#> sum1, carry1 <#> carry2)
  where
    sum1 = a <#> b
    carry1 = a && b
    carry2 = carryIn && sum1

add carryIn [] bs =
  case bs of
    [] -> []
    _ -> bitAdder carryIn bs
add carryIn aas@(a:as) bs =
  case bs of
    [] -> bitAdder carryIn aas
    b:bs -> case fullAdd carryIn a b of
              (r, carry) -> r : add carry as bs

bitAdder cin [] = []
bitAdder cin (a:as) = (a <#> cin) : bitAdder (a && cin) as

sub a b = add True a (map not b)

decrement a = add True a (False:tail (map (const True) a))

le a b = last (sub a b)

rep :: Int -> a -> [a]
rep n a = if n == 0 then [] else a : rep (n-1) a

modulo a b = if a `le` b then a else modulo (a `sub` b) b

divisors n m =
  if or m
  then if or (n `modulo` m) then rest else n : rest
  else []
  where
    rest = divisors n (decrement m)

main :: Int
main = length (divisors n n)
  where
    n = rep 12 False ++ [True]
