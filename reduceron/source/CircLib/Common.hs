module CircLib.Common where

import Lava
import CircLib.Bit
import CircLib.Word
import List

infixl 9 !
infix 4 |<=|
infix 4 |<|

x ! f = f x

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

(?)                     :: Choice a => Bit -> (a, a) -> a
cond ? (th, el)         =  ifThenElse cond (th, el)

flipflop                =  fde False high

rege                    :: [Bool] -> Bit -> [Bit] -> [Bit]
rege i e d              =  lazyZipWith (\i d -> fde i e d) i d

shr                     :: [Bit] -> Int -> [Bit]
shr as n                =  drop n as ++ replicate n low

shl                     :: [Bit] -> Int -> [Bit]
shl as n                =  reverse (shr (reverse as) n)

shre                    :: [Bit] -> Int -> [Bit]
shre as n               =  drop n as ++ replicate n (last as)

tree                    :: (a -> a -> a) -> [a] -> a
tree f [a]              =  a
tree f (a:b:cs)         =  tree f (cs ++ [f a b])

select                  :: [Bit] -> [Word] -> Word
select sels inps        =  map (tree (<|>))
                        $  transpose
                        $  zipWith (\sel -> map (sel <&>)) sels inps

pick                    :: [(Bit, Word)] -> Word
pick choices            =  select sels inps
  where
    (sels, inps)        =  unzip choices

decode                  :: [Bit] -> [Bit]
decode []               =  [high]
decode [x]              =  [inv x, x]
decode (x:xs)           =  concatMap (\y -> [inv x <&> y, x <&> y]) rest
  where
    rest                =  decode xs

fullAdd                 :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdd cin a b         =  (sum, cout)
  where
    sum'                =  a <#> b
    sum                 =  xorcy (sum', cin)
    cout                =  muxcy (sum', (a, cin))

binAdd                  :: Bit -> Word -> Word -> Word
binAdd c a b            =  add c a b
  where
    add c [a]    [b]    =  [sum, cout]
      where
        (sum, cout)     =  fullAdd c a b
    add c (a:as) [b]    =  add c (a:as) [b,b]
    add c [a]    (b:bs) =  add c [a,a] (b:bs)
    add c (a:as) (b:bs) =  sum : add cout as bs
      where
        (sum, cout)     =  fullAdd c a b

(/+/)                   :: Word -> Word -> Word
a /+/ b                 =  init (binAdd low a b)

(/+!/)                  :: Word -> Word -> Word
a /+!/ b                =  init (binAdd high a b)

(/-/)                   :: Word -> Word -> Word
a /-/ b                 =  init (binAdd high a (map inv b))

(/-!/)                  :: Word -> Word -> Word
a /-!/ b                =  init (binAdd low a (map inv b))

(/=/)                   :: Word -> Word -> Bit
a /=/ b                 =  tree (<&>) (zipWith (\a b -> inv (a <#> b)) a b)

(/!=/)                  :: Word -> Word -> Bit
a /!=/ b                =  tree (<|>) (zipWith (<#>) a b)

(/</)                   :: Word -> Word -> Bit
a /</ b                 =  last (a /-/ b)

(/<=/)                  :: Word -> Word -> Bit
a /<=/ b                =  inv (b /</ a)

(|<|)                   :: Word -> Word -> Bit
a |<| b                 =  inv $ last $ binAdd high a (map inv b)

(|<=|)                  :: Word -> Word -> Bit
a |<=| b                =  inv (b |<| a)

isTwo                   :: Word -> Bit
isTwo (a:b:bs)          =  inv a <&> b <&> inv (tree (<|>) bs)

rotl                    :: Int -> [a] -> [a]
rotl n xs               =  drop n xs ++ take n xs

rotr                    :: Int -> [a] -> [a]
rotr n                  =  reverse . rotl n . reverse

rotateLeft              :: [Bit] -> [Word] -> [Word]
rotateLeft n xs         =  map (select (decode n))
                        $  map (`rotl` xs) [0 .. length xs - 1]

rotateRight             :: [Bit] -> [Word] -> [Word]
rotateRight n xs        =  map (select (decode n))
                        $  transpose
                        $  map (`rotr` xs) [0 .. length xs - 1]

rotateRight'            :: [Bit] -> [Word] -> [Word]
rotateRight' n xs       =  map (select n)
                        $  transpose
                        $  map (`rotr` xs) [0 .. length xs - 1]

wordRep                 :: Word -> Word -> [Word]
wordRep n w             =  map make [0 .. (2 ^ length n) - 1]
  where
    make i              =  init $ binAdd ((i `ofWidth` length n) |<| n)
                                         w
                                         (replicate (length w) low)

encode                  :: [Bit] -> [Bit]
encode [_]              =  []
encode as               =  zipWith (<|>) (encode ls) (encode rs)
                        ++ [tree (<|>) rs]
  where
    (ls,rs)             =  splitAt (length as `div` 2) as

orList                  :: [Bit] -> Bit
orList []               =  low
orList xs               =  tree (<|>) xs

onlyFirst               :: [Bit] -> [Bit]
onlyFirst as            =  zipWith (<&>) as inhibiters
  where
    inhibiters          =  map (inv . orList) $ take (length as) $ inits as

firstHigh               :: [Bit] -> [Bit]
firstHigh               =  encode . onlyFirst

mask n                  =  reverse (n `ofWidth` 4)

wordMin                 :: Word -> Word -> Word
wordMin a b             =  (a |<| b) ? (a, b)

minw                    :: Word -> Word -> Word
minw a b                =  (a |<| b) ? (a, b)

--tally                   :: [Bit] -> [Bit]
--tally as                =  map orList $ tail (tails as)

tally                   :: [Bit] -> [Bit]
tally as                =  map (inv . orList) (tail (inits as))
