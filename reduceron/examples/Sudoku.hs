-- "A program to solve Sudoku", Richard Bird, JFP 16(6), 671-679, 2006.

module Sudoku where

del :: Int -> [Int] -> [Int]
del x [] = []
del x (y:ys) = if x == y then ys else y:del x ys

(\\) :: [Int] -> [Int] -> [Int]
xs \\ [] = xs
xs \\ (y:ys) = del y xs \\ ys

undef = undef

hd [] = undef
hd (x:xs) = x

tl [] = undef
tl (x:xs) = xs

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

single :: [a] -> Bool
single [] = False
single (x:xs) = null xs

minim :: [Int] -> Int
minim xs = case xs of
             [] -> undef
             y:ys -> case ys of
                       [] -> y
                       _  -> let m = minim ys
                             in  if y <= m then y else m

brk p [] = ([], [])
brk p xs@(x:xs') =
  if   p x
  then ([],xs)
  else (x:ys, zs)
  where
    (ys,zs) = brk p xs'

filt p [] = []
filt p (x:xs) = if p x then x:filt p xs else filt p xs

zipW f xs ys =
  case xs of
    [] -> []
    x:xs' -> case ys of
               [] -> []
               y:ys' -> f x y : zipW f xs' ys'

--

boardsize, boxsize :: Int
boardsize = 9
boxsize   = 3

type Choices = [Int]

cellvals :: Choices
cellvals  = [1,2,3,4,5,6,7,8,9]

blank :: Int -> Bool
blank x = x == 0

type Matrix a = [[a]]
type Board    = Matrix Int

nodups :: [Int] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs

cols :: Matrix a -> Matrix a
cols [] = undef
cols (xs:as) =
  case as of
    [] -> [[x] | x <- xs]
    xss -> zipW (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map concat . concat . map cols . groupBy 3 . map (groupBy 3)

{-
groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = nxs : groupBy n etc
  where
  (nxs, etc) = splitAt n xs
-}

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

choices :: Board -> Matrix Choices
choices = map (map choose)

choose e = if blank e then cellvals else [e]

fixed :: [Choices] -> Choices
fixed = concat . filt single

reduce :: [Choices] -> [Choices]
reduce css = map (remove (fixed css)) css

remove :: Choices -> Choices -> Choices
remove fs cs = if single cs then cs else cs \\ fs

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy id

pruneBy :: (Matrix Choices -> Matrix Choices) ->
           (Matrix Choices -> Matrix Choices)
pruneBy f = f . map reduce . f

blocked :: Matrix Choices -> Bool
blocked cm = void cm || not (safe cm)

void :: Matrix Choices -> Bool
void = any (any null)

safe :: Matrix Choices -> Bool
safe cm = all (nodups.fixed) cm &&
          all (nodups.fixed) (cols cm) &&
          all (nodups.fixed) (boxs cm)

expand cm = [ rows1++[row1++[c]:tl row2]++tl rows2 | c <- hd row2 ]
  where
  (rows1, rows2)     = brk (any best) cm
  (row1,row2)        = brk best (hd rows2)
  best cs            = len cs == n
  n                  = minchoice cm

minchoice = minim . filt p . concat . map (map len)
  where
    p :: Int -> Bool
    p x = 2 <= x

search :: Matrix Choices -> [Matrix Choices]
search cm =
  if blocked cm then []
  else if all (all single) cm then [cm]
  else (concat . map (search.prune) . expand) cm


sudoku :: Board -> [Board]
--sudoku = map (map (map hd)) . search . prune . choices
sudoku = map (map (map hd)) . search . prune . choices

-- IO & main program

{-
problem :: Matrix Int
problem = [ [2, 0, 0, 0, 0, 1, 0, 3, 8]
          , [0, 0, 0, 0, 0, 0, 0, 0, 5]
          , [0, 7, 0, 0, 0, 6, 0, 0, 0]
          , [0, 0, 0, 0, 0, 0, 0, 1, 3]
          , [0, 9, 8, 1, 0, 0, 2, 5, 7]
          , [3, 1, 0, 0, 0, 0, 8, 0, 0]
          , [9, 0, 0, 8, 0, 0, 0, 2, 0]
          , [0, 5, 0, 0, 6, 9, 7, 8, 4]
          , [4, 0, 0, 2, 5, 0, 0, 0, 0]
          ]
-}

{-
problem :: Matrix Int
problem = [ [0,0,0,0,0,3,0,6,0]
          , [0,0,0,0,0,0,0,1,0]
          , [0,9,7,5,0,0,0,8,0]
          , [0,0,0,0,9,0,2,0,0]
          , [0,0,8,0,7,0,4,0,0]
          , [0,0,3,0,6,0,0,0,0]
          , [0,1,0,0,0,2,8,9,0]
          , [0,4,0,0,0,0,0,0,0]
          , [0,5,0,1,0,0,0,0,0]
          ]
-}

main = if null sols then 0 else sumList (map sumList (hd sols))
  where
    sols = sudoku problem

    problem :: Matrix Int
    problem = [ [0,0,0,0,0,3,0,6,0]
          , [0,0,0,0,0,0,0,1,0]
          , [0,9,7,5,0,0,0,8,0]
          , [0,0,0,0,9,0,2,0,0]
          , [0,0,8,0,7,0,4,0,0]
          , [0,0,3,0,6,0,0,0,0]
          , [0,1,0,0,0,2,8,9,0]
          , [0,4,0,0,0,0,0,0,0]
          , [0,5,0,1,0,0,0,0,0]
          ]
