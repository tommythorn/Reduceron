{-# OPTIONS_GHC -XFlexibleInstances #-}

{-|

The beginnings of a Prelude of commonly-used circuits.  By no means
exhaustive, but a useful start.

-}

module Lava.Prelude
  ( -- * Bit-vectors
    Word

    -- * Generalised primitives
  , andG
  , orG
  , delay
  , delayEn
  , (?)
  , nameList
  , nameWord

    -- * Multiplexing
  , select
  , selectG
  , pick
  , pickG

    -- * Encoding and decoding
  , decode
  , decodeTwos
  , encode
  , tally
  , oneHot
  , tal
  , tal'

    -- * Rotation
  , rotr
  , rotateRight
  , rotl
  , rotateLeft
  , dot

    -- * RAMs
  , RamInputs(..)
  , ram
  , dualRam

    -- * Arithmetic
  , Unsigned
  , Signed(..)
  , natSub
  , complement
  , bitPlus
  , wordToInt
  , extend

    -- * Comparators
  , (===)
  , (=/=)
  , Ordered(..)

    -- * Polymorphic functions over lists
  , tree1
  , tree
  , groupN
  , halve
  ) where

import Lava.Bit
import Lava.Vector
import Lava.Binary
import Data.List(transpose, inits, tails)

-- | Parallel reduce for a commutative an associative operator.  Input
-- list must be non-empty.
tree1 :: (a -> a -> a) -> [a] -> a
tree1 f [x] = x
tree1 f (x:y:ys) = tree1 f (ys ++ [f x y])

-- | Like 'tree1', but input list may be empty, in which case the zero
-- element is returned.
tree :: (a -> a -> a) -> a -> [a] -> a
tree f z xs = if null xs then z else tree1 f xs

-- | Split a list into sub-lists of maximum length N.
groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)

-- | Logical AND of all bits in a structure.
andG :: Generic a => a -> Bit
andG = tree (<&>) high . bits

-- | Logical OR of all bits in a structure.
orG :: Generic a => a -> Bit
orG = tree (<|>) low . bits

infix 4 ===
-- | Generic equality.
(===) :: Generic a => a -> a -> Bit
a === b = andG $ bits $ zipWithG (<=>) a b

infix 4 =/=
-- | Generic diseqaulity.
(=/=) :: Generic a => a -> a -> Bit
a =/= b = inv $ andG $ bits $ zipWithG (<=>) a b

-- | Generic register, with initialiser.
delay :: Generic a => a -> a -> a
delay init a = lazyZipWithG delayBit init a

-- | Generic register, with initialiser, with input-enable.
delayEn :: Generic a => a -> Bit -> a -> a
delayEn init en a = lazyZipWithG (delayBitEn en) init a

-- | Generic two-way multiplexer.
(?) :: Generic a => Bit -> (a, a) -> a
cond ? (a, b) = zipWithG (muxBit cond) b a

-- | N-way multiplexer, with one-hot address.
select :: [Bit] -> [[Bit]] -> [Bit]
select sels inps = map orG
                 $ transpose
                 $ zipWith (\sel -> map (sel <&>)) sels inps

-- | Generic 'select'.
selectG :: Generic a => [Bit] -> [a] -> a
selectG sels inps = tree1 (zipWithG (<|>))
                  $ zipWith (\sel -> mapG (sel <&>)) sels inps

-- | Like 'select', but with zipped arguments.
pick :: [(Bit, [Bit])] -> [Bit]
pick choices = select sels inps
  where (sels, inps) = unzip choices

-- | Generic 'pick'.
pickG :: Generic a => [(Bit, a)] -> a
pickG choices = selectG sels inps
  where (sels, inps) = unzip choices

-- | Binary to one-hot decoder.
decode :: [Bit] -> [Bit]
decode [] = [high]
decode [x] = [inv x, x]
decode (x:xs) = concatMap (\y -> [inv x <&> y, x <&> y]) rest
  where rest = decode xs

-- | Two's complement version of 'decode'.
decodeTwos :: [Bit] -> [Bit]
decodeTwos xs = zipWith (<|>) ys zs
  where (ys, zs) = halve (decode xs)

-- | Split a list in two.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- | One-hot to binary encoder.
encode :: [Bit] -> [Bit]
encode [_] = []
encode as  = zipWith (<|>) (encode ls) (encode rs) ++ [orG rs]
  where (ls,rs) = splitAt (length as `div` 2) as

-- | Binary to tally converter.
tally :: [Bit] -> [Bit]
tally = tal . decode

-- | One-hot to tally converter.
tal :: [Bit] -> [Bit]
tal = map orG . tail . tails

-- | Like 'tal'; specifically @tal\' n  =  tal (n+1)@.
tal' :: [Bit] -> [Bit]
tal' = map orG . init . tails

split :: [a] -> [([a], [a])]
split [] = []
split (x:xs) = ([x], xs) : [(x:y, z) | (y, z) <- split xs]

tac :: ([a], [a]) -> [a]
tac (xs, ys) = reverse xs ++ reverse ys

-- | Dot product over bit-lists.
dot :: [Bit] -> [Bit] -> Bit
dot xs ys = orG (zipWith (<&>) xs ys)

-- | Rotate @b@ by @a@ places to the right; @a@ is a one-hot number.
rotr :: [Bit] -> [Bit] -> [Bit]
rotr a b = map (dot a) (map tac (split b))

-- | Like 'rotr', but lifted to a list of bit-lists.
rotateRight :: [Bit] -> [[Bit]] -> [[Bit]]
rotateRight n = transpose . map (rotr n) . transpose

-- | Like 'rotr', except rotation is to the left.
rotl :: [Bit] -> [Bit] -> [Bit]
rotl (a:as) b = rotr (a:reverse as) b

-- | Like 'rotateRight' except rotation is to the left.
rotateLeft :: [Bit] -> [[Bit]] -> [[Bit]]
rotateLeft n = transpose . map (rotl n) . transpose

-- | Sign-extend a bit-vector.
extend :: N n => Vec (S m) c -> Vec n c
extend n = vextend (vlast n) n

intToOneHot :: Int -> Int -> [Bit]
intToOneHot i w
  | i < 0 = reverse bits
  | otherwise = bits
  where bits = [if abs i == j then high else low | j <- [0..w-1]]

-- | Convert a Haskell @Int@ to a one-hot bit-vector.
oneHot :: N n => Int -> Word n
oneHot i = sized (Vec . intToOneHot i)

------------------------------------- RAMs ------------------------------------

data RamInputs n m =
  RamInputs {
    ramData    :: Word n
  , ramAddress :: Word m
  , ramWrite   :: Bit
  }

-- | RAM of any width and size, with intialiser.
ram :: (N n, N m) => [Integer] -> RamAlgorithm -> RamInputs n m -> Word n
ram init pt inps = Vec $ primRam init pt $
  RamInps {
      dataBus     = velems (vrigid $ ramData inps)
    , addressBus  = velems (vrigid $ ramAddress inps)
    , writeEnable = ramWrite inps
  }

-- | Dual-port RAM of any width and size, with intialiser.
dualRam :: (N n, N m) => [Integer] -> RamAlgorithm
        -> (RamInputs n m, RamInputs n m) -> (Word n, Word n)
dualRam init pt (inps0, inps1) = (Vec out0, Vec out1)
  where
    (out0, out1) =
      primDualRam init pt
        ( RamInps {
            dataBus     = velems (vrigid $ ramData inps0)
          , addressBus  = velems (vrigid $ ramAddress inps0)
          , writeEnable = ramWrite inps0
          }
        , RamInps {
            dataBus     = velems (vrigid $ ramData inps1)
          , addressBus  = velems (vrigid $ ramAddress inps1)
          , writeEnable = ramWrite inps1
          }
        )

---------------------------------- Arithmetic ---------------------------------

fullAdd :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdd cin a b = (sum, cout)
  where sum' = a <#> b
        sum  = xorcy (sum', cin)
        cout = muxcy sum' (a, cin)

binAdd :: Bit -> [Bit] -> [Bit] -> [Bit]
binAdd c a b = add c a b
  where
    add c [a]    [b]    = [sum, cout]
      where (sum, cout) = fullAdd c a b
    add c (a:as) [b]    = add c (a:as) [b,b]
    add c [a]    (b:bs) = add c [a,a] (b:bs)
    add c (a:as) (b:bs) = sum : add cout as bs
      where (sum, cout) = fullAdd c a b

infixl 6 /+/
(/+/) :: [Bit] -> [Bit] -> [Bit]
a /+/ b = init (binAdd low a b)

infixl 6 /-/
(/-/) :: [Bit] -> [Bit] -> [Bit]
a /-/ b = init (binAdd high a (map inv b))

infix 4 /</
(/</) :: [Bit] -> [Bit] -> Bit
a /</ b = last (a /-/ b)

infix 4 /<=/
(/<=/) :: [Bit] -> [Bit] -> Bit
a /<=/ b = inv (b /</ a)

infix 4 />/
(/>/) :: [Bit] -> [Bit] -> Bit
a />/ b = b /</ a

infix 4 />=/
(/>=/) :: [Bit] -> [Bit] -> Bit
a />=/ b = b /<=/ a

ult :: [Bit] -> [Bit] -> Bit
a `ult` b = inv $ last $ binAdd high a (map inv b)

ule :: [Bit] -> [Bit] -> Bit
a `ule` b = inv (b `ult` a)

ugt :: [Bit] -> [Bit] -> Bit
a `ugt` b = b `ult` a

uge :: [Bit] -> [Bit] -> Bit
a `uge` b = b `ule` a

-- | Two's complement of a bit-list.
complement :: [Bit] -> [Bit]
complement a = init $ binAdd high (map inv a) [low]

-- | Addition of a single bit to a bit-list.
bitPlus :: Bit -> [Bit] -> [Bit]
bitPlus a b = init (binAdd a (map (const low) b) b)

---------------------------------- Bit Vectors --------------------------------

instance Generic a => Generic (Vec n a) where
  generic (Vec []) = cons (Vec [])
  generic (Vec (x:xs)) = cons (\x xs -> Vec (x:xs)) >< x >< xs

-- | Notably, an instance of the Num class.
type Word n = Vec n Bit

-- | Unsigned bit-vectors.
type Unsigned n = Word n

-- | Convert bit-vector to an integer.
wordToInt :: Integral a => Word n -> a
wordToInt = binToNat . map bitToBool . velems

instance Eq (Vec n Bit) where
  a == b = error msg
    where msg = "== and /= on bit-vectors is not supported: try === and =/="

instance N n => Num (Vec n Bit) where
  a + b = vec (velems a /+/ velems b)
  a - b = vec (velems a /-/ velems b)
  a * b = error "Multiplication of bit-vectors is not yet supported"
  abs a = a
  -- just 0 or 1 as vectors are interpreted as unsigned
  signum v = vec (orG xs : repeat 0)
    where xs = velems v
  fromInteger i = sized (\n -> Vec (fromInteger i `ofWidth` n))

ofWidth :: Integral a => a -> Int -> [Bit]
n `ofWidth` s = map boolToBit (intToSizedBin n s)

infix 4 |<=|, |<|, |>|, |>=|

class Ordered a where
  (|<=|) :: a -> a -> Bit
  (|<|)  :: a -> a -> Bit
  (|>=|) :: a -> a -> Bit
  (|>|)  :: a -> a -> Bit

instance Ordered (Vec n Bit) where
  a |<=| b = velems a `ule` velems b
  a |<| b  = velems a `ult` velems b
  a |>=| b = velems a `uge` velems b
  a |>| b  = velems a `ugt` velems b

-- | Subtracts @b@ from @a@, but if @b@ is larger than @a@ then
-- result is @0@.
natSub :: N n => Word n -> Word n -> Word n
natSub a b = Vec $ mapG (last r <&>) (init r)
  where (x, y) = (velems a, velems b)
        r = binAdd high x (map inv y)

------------------------------ Signed Bit Vectors -----------------------------

-- | Signed bit-vectors.
newtype Signed n = Signed (Vec n Bit)
  deriving Show

instance Generic (Signed n) where
  generic (Signed n) = cons Signed >< n

instance Eq (Signed n) where
  a == b = error msg
    where msg = "== and /= on bit-vectors is not supported: try === and =/="

instance N n => Num (Signed n) where
  Signed a + Signed b = Signed (a + b)
  Signed a - Signed b = Signed (a - b)
  a * b = error "(*) on bit-vectors is not yet supported"
  abs (Signed a) = last (velems a) ? (negate (Signed a), Signed a)
  signum (Signed a) = error "signum on bit-vectors is not yet supported"
  fromInteger i = Signed $ sized (\n -> Vec (fromInteger i `ofWidth` n))

instance Ordered (Signed n) where
  Signed a |<=| Signed b = ext1 (velems a) /<=/ ext1 (velems b)
  Signed a |<|  Signed b = ext1 (velems a) /</  ext1 (velems b)
  Signed a |>=| Signed b = ext1 (velems a) />=/ ext1 (velems b)
  Signed a |>|  Signed b = ext1 (velems a) />/  ext1 (velems b)

ext1 :: [Bit] -> [Bit]
ext1 [] = [low]
ext1 xs = xs ++ take 1 (reverse xs)

-- | Returns a list of N named bits with a given prefix.
nameList :: Int -> String -> [Bit]
nameList n s = map (name . (s ++) . show) [1..n]

-- | Returns a vector of N named bits with a given prefix.
nameWord :: N n => String -> Word n
nameWord s = sized (\n -> Vec $ nameList n s)

instance Eq Bit where
  a == b = error "== and /= on bits is not supported."

instance Num Bit where
  a + b = a <#> b
  a - b = a <&> inv b
  a * b = a <&> b
  abs a = a
  signum a = a
  fromInteger i = if i == 0 then low else high
