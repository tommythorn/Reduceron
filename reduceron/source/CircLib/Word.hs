module CircLib.Word (Word, ofWidth, unsigned) where

import Lava
import CircLib.Bit

type Word        =  [Bit]

ofWidth          :: Int -> Int -> Word
n `ofWidth` w
  | n < 0        =  (map bit . extend w True . complement . bin . abs) n
  | otherwise    =  (map bit . extend w False . bin) n

unsigned a       =  (a `ofWidth` len) ++ [low]
  where
    len          =  length (bin a)

bin              :: Int -> [Bool]
bin 0            =  []
bin n            =  (n `mod` 2 == 1) : bin (n `div` 2)

complement       :: [Bool] -> [Bool]
complement       =  boolAdd True . map not

boolAdd          :: Bool -> [Bool] -> [Bool]
boolAdd a []     =  []
boolAdd a (b:bs) =  (a /= b) : boolAdd (a && b) bs

extend           :: Int -> Bool -> [Bool] -> [Bool]
extend w a bs    =  bs ++ replicate (w - length bs) a
