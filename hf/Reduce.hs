module Reduce where

infixl 5 >>>   -- Using >>> instead of >> to avoid problems with 1.3 Prelude

type Reduce  s   s' = s -> s'

unitR :: Reduce s s
unitR = id

(>>>) :: Reduce s s' -> Reduce s' s'' -> Reduce s s''
f >>> g = g . f

mapR :: (a->Reduce s s) -> [a] -> Reduce s s
mapR _f []     = unitR
mapR f (x:xs) = f x >>> mapR f xs
