module Hex where

binToInt :: [Bool] -> Integer
binToInt []         = 0
binToInt (False:xs) = 2 * binToInt xs
binToInt (True:xs)  = 1 + (2 * binToInt xs)

binToHex :: [Bool] -> String
binToHex [] = ""
binToHex xs = hexit (binToInt (take 4 xs)) : binToHex (drop 4 xs)

hexit :: Integer -> Char
hexit n = if n <= 9 then toEnum (fromEnum '0' + fromIntegral n)
                    else toEnum (fromEnum 'A' + fromIntegral n - 10)

intToBin :: Integer -> [Bool]
intToBin 0 = []
intToBin n = ((n `mod` 2) /= 0) : intToBin (n `div` 2)

intToHex :: Integer -> String
intToHex = reverse . binToHex . intToBin

hex :: Int -> Integer -> String
hex w n = replicate (w - length s) '0' ++ s
  where s = intToHex n
