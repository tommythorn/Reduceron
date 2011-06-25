module CircLib.NumberBase where

import Data.Char

binToInt :: [Bool] -> Int
binToInt []         = 0
binToInt (False:xs) = 2 * binToInt xs
binToInt (True:xs)  = 1 + (2 * binToInt xs)

binToHex :: [Bool] -> String
binToHex [] = ""
binToHex xs = hexit (binToInt (take 4 xs)) : binToHex (drop 4 xs)
  where hexit n = if n <= 9 then chr (ord '0' + n)
                            else chr (ord 'A' + n - 10)

intToBin :: Int -> [Bool]
intToBin 0 = []
intToBin n = ((n `mod` 2) /= 0) : intToBin (n `div` 2)

intToHex :: Int -> String
intToHex = reverse . binToHex . intToBin

hex :: Int -> Int -> String
hex w n = replicate (w - length s) '0' ++ s
  where
    s = intToHex n
