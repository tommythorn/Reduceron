module Lava.Binary where

import Data.Char

binToNat :: Integral a => [Bool] -> a
binToNat [] = 0
binToNat (x:xs) = 2 * binToNat xs + if x then 1 else 0

natToBin :: Integral a => a -> [Bool]
natToBin 0 = []
natToBin n = odd n : natToBin (n `div` 2)

binToHex :: [Bool] -> String
binToHex [] = ""
binToHex xs = hexit (binToNat (take 4 xs)) : binToHex (drop 4 xs)
  where hexit n = if n <= 9 then chr (ord '0' + fromIntegral n)
                            else chr (ord 'A' + fromIntegral n - 10)

natToHex :: Integral a => a -> String
natToHex = reverse . binToHex . natToBin

hex :: Integral a => Int -> a -> String
hex w n = replicate (w - length s) '0' ++ s
  where s = natToHex n

ext :: Int -> a -> [a] -> [a]
ext w x xs = take w (xs ++ repeat x)

natToSizedBin :: Integral a => a -> Int -> [Bool]
natToSizedBin n s = ext s False (natToBin n)

twosComplement :: [Bool] -> [Bool]
twosComplement = boolAdd True . map not

boolAdd :: Bool -> [Bool] -> [Bool]
boolAdd a [] = []
boolAdd a (b:bs) = (a /= b) : boolAdd (a && b) bs

intToBin :: Integral a => a -> [Bool]
intToBin n
  | n >= 0 = natToBin n
  | otherwise = twosComplement $ natToBin $ abs n

intToSizedBin :: Integral a => a -> Int -> [Bool]
intToSizedBin n s = ext s (n < 0) (intToBin n)

binToInt :: Integral a => [Bool] -> a
binToInt bs
  | last bs = negate $ binToNat $ twosComplement bs
  | otherwise = binToNat bs

log2 :: Integral a => a -> a
log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)

rol :: [a] -> Int -> [a]
rol xs n = drop n xs ++ take n xs

ror :: [a] -> Int -> [a]
ror xs n = reverse $ (`rol` n) $ reverse xs
