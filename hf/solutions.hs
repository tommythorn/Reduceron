import Prelude hiding (sum, take, drop, zipWith)

-- SUM
-- It falls out as a simple list recursion.

sum :: [Int] -> Int
sum []      =  0
sum (x:xs)  =  x + sum xs

-- APPEND
-- There is no need for a case analysis on both
-- arguments; just the first will do.

append :: [a] -> [a] -> [a]
append []     ys  =  ys
append (x:xs) ys  =  x : append xs ys

-- TAKE
-- This solution allows negative n, with the
-- same result as if n=0.

take :: Int -> [a] -> [a]
take n []      =  []
take n (x:xs)  =  if n < 1 then [] else x : take (n-1) xs

-- DROP
-- The same approach as for TAKE.

drop :: Int -> [a] -> [a]
drop n []      =  []
drop n (x:xs)  =  if n < 1 then x:xs else drop (n-1) xs

-- CONTAINS
-- Using non-strict || is just a nicety.  There's also a single-equation
-- definition using && too.

contains :: Eq a => [a] -> a -> Bool
contains []     y  =  False
contains (x:xs) y  =  x==y || contains xs y

-- SET
-- Compare this with contains.  Again there's a single-equation definition
-- using both && and ||.

set :: Eq a => [a] -> Bool
set []      =  True
set (x:xs)  =  not (contains xs x) && set xs

-- POSITIONS
-- The required function numbers positions starting from 1.
-- A straightforward generalisation numbers them starting
-- from s, which is introduced as an auxiliary argument.

positions :: Eq a => [a] -> a -> [Int]
positions xs i  =  posStarting 1 xs i

posStarting s []     i  =  []
posStarting s (x:xs) i  =  if x==i then s:p else p
  where
  p  =  posStarting (s+1) xs i

-- DUPLICATES
-- There are lots of other ways to solve this one - nearly all
-- of them more complicated!

duplicates :: Eq a => [a] -> [a]
duplicates []      =  []
duplicates (x:xs)  =
  if not (contains d x) && contains xs x then x:d else d 
  where
  d  =  duplicates xs

-- SORT
-- Perhaps the simplest solution is an insertion sort.  It is reached
-- by asking the usual recursive question, and fits the foldr pattern.

sort :: Ord a => [a] -> [a]
sort  =  foldr insert []

insert n []      =  [n]
insert n (x:xs)  =  if n<=x then n: x: xs else x: insert n xs

-- ZIPWITH
-- The asymmetry in the left-hand sides of this definition
-- is a price we pay to ensure that equation-order doesn't
-- matter.
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith f []     ys      =  []
zipWith f (x:xs) []      =  []
zipWith f (x:xs) (y:ys)  =  f x y : zipWith f xs ys

-- TRANSPOSE
-- map and zipWith provide all the auxiliary power that's needed.

transpose :: [[a]] -> [[a]]
transpose [r]     =  map (:[]) r
transpose (r:rs)  =  zipWith (:) r (transpose rs)

-- POWERSET
-- Remembering/realising that the size of the power set is a power
-- of two gives the strong hint that the recursive formulation must
-- double the size of the result.  The two ways of using each subset
-- from the recursive application are: (1) any subset of xs is a subset
-- of (x:xs) also, and (2) it will remain so if x is added to it.

powerset :: [a] -> [[a]]
powerset []      =  [[]]
powerset (x:xs)  =  p ++ map (x:) p
  where
  p  =  powerset xs

-- The Triangle type is just a synonym for a list of lists  but signals
-- the intention that the nth inner list has n elements.

type Triangle a = [[a]]

-- TROUT
-- One solution generalises the problem by enriching the result to
-- compute the order of the triangle as well as its display.
-- This is useful because each row can appropriately be indented by
-- the same number of spaces as there are subsequent rows (assuming
-- that elements print as single characters).

trout :: Show a => Triangle a -> IO ()
trout  =  putStr . fst . trout'
  where
  trout'       =  foldr row ("", 0)
  row r (d, n) =  (replicate n ' ' ++ foldr col ('\n':d) r, n+1)
  col c d      =  show c ++ " " ++ d

-- PASCAL
-- The full (infinite) Pascal's triangle is a useful auxiliary!

pascal :: Int -> Triangle Int
pascal n  =  take n fullPascal
  where
  fullPascal  =  [1] : map rowAfter fullPascal
  rowAfter r  =  [1] ++ zipWith (+) r (tail r) ++ [1]

-- The Frustum type synonym indicates a generalisation of Triangle
-- where the nth inner list has n+k elements for some constant k>=0.

type Frustum a = [[a]]

-- TROL
-- A non-empty triangle decomposes into an edge and a smaller triangle.
-- The frustum accumulator in this solution acts as a platform for the
-- triangle to rotate onto.

trol :: Triangle a -> Triangle a
trol = trolOnto []

trolOnto :: Frustum a -> Triangle a -> Triangle a
trolOnto f []      =  f
trolOnto f (r:rs)  =  trolOnto (map head (r:rs) : f) (map tail rs)

-- TROR
-- The following solution inverts the pattern of computation in
-- trol.  So the accumulator is now a triangle and the recursively
-- decomposed argument is a frustum.

tror :: Triangle a -> Triangle a
tror = frorInto []

frorInto :: Triangle a -> Frustum a -> Triangle a
frorInto t []      =  t
frorInto t (r:rs)  =  frorInto (zipWith (:) r ([]:t)) rs

-- HAMMING
-- This solution closely follows the hint given in the question.
-- Only one equation is needed for merge: since both arguments are
-- infinite lists there is no need to cater for [].

hamming = h 2 `merge` h 3 `merge` h 5

h n = map (n*) (1 : hamming)
  
merge (x:xs) (y:ys) =
  if      x < y then x : merge xs (y:ys)
  else if y < x then y : merge (x:xs) ys
  else x : merge xs ys
  
-- PRIMES
-- A common definition based on the "Sieve of Eratosthenes" method
-- introduces for each prime a new filter to eliminate its products.
-- This solution is much faster, as there is only one filter.

primes  =  sieve [2..]
  where
  sieve (p:xs)         =  p : filter (noFactorIn primes) xs
  noFactorIn (p:ps) x  =  p*p > x || x `mod` p > 0 && noFactorIn ps x

-- ACKERMANN
-- Since ack has two arguments, the memo structure ack' is an infinite
-- matrix (list of lists) of results, indexed by first argument as
-- row and second argument as column.

ack :: Int -> Int -> Int
ack 0     n      =  n+1
ack (m+1) 0      =  ack' !! m !! 1
ack m     (n+1)  =  ack' !! (m-1) !! (ack' !! m !! n)

ack' :: [[Int]]
ack'  =  table (table . ack)

table :: (Int->a) -> [a]
table f  =  map f [0..]

-- SEGMENTS
-- One definition of a segment is the prefix of a suffix.  Using a
-- comprehension gives a one-liner to list them all.

segments :: [a] -> [[a]]
segments xs  =  [p | s <- suffixes xs, p <- prefixes s, not (null p)]

prefixes :: [a] -> [[a]]
prefixes []      =  [[]]
prefixes (x:xs)  =  [] : map (x:) (prefixes xs)

suffixes :: [a] -> [[a]]
suffixes []      =  [[]]
suffixes (x:xs)  =  (x:xs) : suffixes xs

-- CHANGE
-- This solution drops the coins into each bag in ascending order
-- of value.  The comprehension chooses a first coin value c, which
-- clearly cannot be more than m. The rest of the bag is filled
-- with some recursively computed combination of coins (>= c) making
-- up the outstanding amount (m-c).

change :: [Int] -> Int -> [[Int]]
change cs 0  =  [[]]
change cs m  =  [c : b | c <- cs, c <= m,
                         b <- change (filter (>= c) cs) (m-c)]

-- PERMS
-- There are many possible definitions.  The nicest are purely
-- structural solutions: they do not require comparisons of elements.
-- The picks function here lists all ways of picking out one element
-- from a list, each giving an (element,list-remaining) pair.

perms :: [a] -> [[a]]
perms []      =  [[]]
perms xs      =  [x:p | (x,xs') <- picks xs, p <- perms xs']

picks :: [a] -> [(a,[a])]
picks []      =  []
picks (x:xs)  =  (x,xs) : [(x',x:xs') | (x',xs') <- picks xs]

-- SYLLABLES
-- What's a syllable?  The syllable counting function syl here
-- uses a rough-and-ready rule only.  The body of syllables is
-- expressed using pipelines of composed functions, both for
-- processing the input as a whole and for processing each line.
-- Read pipelines right-to-left for stages between input and output.

syllables :: String -> String
syllables  =  concat . interleave (repeat "word: ") .
              map ((++" syllable(s)\n") . show . syl) . lines
  where
  syl [a]      =  bit (vowel a)
  syl (a:b:w)  =  bit (vowel a && not (vowel b)) + syl (b:w)
  bit True     =  1
  bit False    =  0
  vowel        =  contains "aeiouy"

interleave :: [a] -> [a] -> [a]
interleave []     _   =  []
interleave (x:xs) ys  =  x : interleave ys xs

-- WORDCOUNT
-- And what's a word?  Another rough-and-ready rule here:
-- the number of words on a line is the number of blanks
-- plus one.

wordcount :: String -> String
wordcount  =  unlines . mapState addWords 0 . lines
  where
  addWords t s  =  (t', show t'++" WORDS")
    where
    t'  =  1 + length (filter (==' ') s) + t

mapState :: (a->b->(a,c)) -> a -> [b] -> [c]
mapState f s []      =  []
mapState f s (x:xs)  =  y : mapState f t xs
  where
  (t,y)  =  f s x
