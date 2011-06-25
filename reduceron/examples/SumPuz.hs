module SumPuz where

type Soln = [(Int, Int)]

rng :: Soln -> [Int]
rng = map snd

img :: Soln -> Int -> Int
img lds l = fromJust (look l lds)

look :: Int -> [(Int,a)] -> Maybe a
look a [] = Nothing
look a ((b,c):rest) = if a == b then Just c else look a rest

fromJust (Just a) = a
fromJust Nothing = fromJust Nothing

sumlist :: [Int] -> Int
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

del :: Int -> [Int] -> [Int]
del _ []     =  []
del x (y:ys) = if x == y then ys else y : del x ys

member :: Int -> [Int] -> Bool
member x [] = False
member x (y:ys) = if x == y then True else member x ys

(\\) :: [Int] -> [Int] -> [Int]
(\\) =  foldl (flip del)

bindings :: Int -> [Int] -> Soln -> [Soln]
bindings l ds lds =
  case look l lds of
  Nothing  -> map (:lds) (zip (repeat l) (ds \\ rng lds))
  Just d -> if d `member` ds then [lds] else []

solutions :: [Int] -> [Int] -> [Int] -> Int -> Soln -> [Soln]
solutions [] [] []  c lds = if c==0 then [lds] else []
solutions [] [] [z] c lds = if c==1 then bindings z [1] lds else []
solutions (x:xs) (y:ys) (z:zs) c lds =
  solns `ofAll`
  bindings y (fromTo (if null ys then 1 else 0) 9) `ofAll`
  bindings x (fromTo (if null xs then 1 else 0) 9) lds
  where  
  solns s = 
    solutions xs ys zs q `ofAll` bindings z [r] s
    where    
    xy = img s x + img s y + c
    (q, r) = divMod10 xy

divMod10 :: Int -> (Int, Int)
divMod10 n = if n <= 9 then (0, n) else (q+1, r)
  where
    q, r :: Int
    (q, r) = divMod10 (n-10)

infixr 5 `ofAll`
ofAll :: (a -> [b]) -> [a] -> [b]
ofAll = concatMap

fromTo :: Int -> Int -> [Int]
fromTo n m = n : if n == m then [] else fromTo (n+1) m

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

main :: Int
main =  len (solutions wales italy spain 0 [])
     +  len (solutions spain italy paris 0 [])
     +  len (solutions spain malta paris 0 [])
     +  len (solutions norway prague europe 0 [])
     +  len (solutions andora prague england 0 [])
     +  len (solutions sicily london andora 0 [])
  where
    wales, italy, spain, paris, malta,
      norway, prague, europe, andora, england,
      sicily, london  :: [Int]
    wales = [87,65,76,69,83]
    italy = [73,84,65,76,89]
    spain = [83,80,65,73,78]
    paris = [83,73,82,65,80]
    malta = [65,84,76,65,77]
    norway = [89,65,87,82,79,78]
    prague = [69,85,71,65,82,80]
    europe = [69,80,79,82,85,69]
    andora = [65,82,79,68,78,65]
    england = [68,78,65,76,71,78,69]
    sicily = [89,76,73,67,73,83]
    london = [78,79,68,78,79,76]
