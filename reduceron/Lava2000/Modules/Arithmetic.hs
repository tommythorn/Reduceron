module Arithmetic where

import Lava
import Patterns

----------------------------------------------------------------
-- Basic Components

halfAdd (a, b) = (sum, carry)
  where
    sum   = xor2 (a, b)
    carry = and2 (a, b)

fullAdd (carryIn, (a, b)) = (sum, carryOut)
  where
    (sum1, carry1) = halfAdd (a, b)
    (sum, carry2)  = halfAdd (carryIn, sum1)
    carryOut       = xor2 (carry1, carry2)

bitAdder = row halfAdd

adder (carryIn, ([],   []))   = ([], carryIn)
adder (carryIn, (as,   []))   = bitAdder (carryIn, as)
adder (carryIn, ([],   bs))   = bitAdder (carryIn, bs)
adder (carryIn, (a:as, b:bs)) = (s:ss, carryOut)
  where
    (s, carry)     = fullAdd (carryIn, (a, b))
    (ss, carryOut) = adder (carry, (as, bs))

binAdder (as, bs) = sum ++ [carryOut]
  where
    (sum, carryOut) = adder (low, (as, bs))

bitMulti (a, bs) = [ and2 (a, b) | b <- bs ]

mult ([],   []) = []
mult (as,   []) = replicate (length as) low
mult ([],   bs) = replicate (length bs) low
mult (a:as, bs) = m : ms
  where
    (m:abs) = bitMulti (a, bs)
    asbs    = mult (as, bs)
    (ms,_)  = adder (low, (abs, asbs))

numBreak num = (bit, num')
  where
    digit = imod (num, 2)
    bit   = int2bit digit
    num'  = idiv (num, 2)

int2bin 0 num = []
int2bin n num = (bit:bits)
  where
    (bit,num') = numBreak num
    bits       = int2bin (n-1) num'

bin2int []     = 0
bin2int (b:bs) = num
  where
    num' = bin2int bs
    num  = bit2int b + 2 * num'

----------------------------------------------------------------
-- the end.


