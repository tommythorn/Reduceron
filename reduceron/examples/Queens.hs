module Queens where

main = nsoln 10

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

nsoln :: Int -> Int
nsoln nq = len (gen nq)
  where
    gen :: Int -> [[Int]]
    gen 0 = [[]]
    gen n = [ (q:b) | b <- gen (n-1), q <- toOne nq, safe q 1 b]

safe :: Int -> Int -> [Int] -> Bool
safe x d []    = True
safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

toOne :: Int -> [Int]
toOne n = if n == 1 then [1] else n : toOne (n-1)
