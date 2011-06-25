module Patterns where

import Lava

infixr 5 ->-
infixr 4 -|-

{-
This Lava module defines some often used wiring circuits
and connection patterns.
-}

----------------------------------------------------------------
-- Wiring Circuits

swap  (a,b) = (b,a)
swapl [a,b] = [b,a]

copy a      = (a,a)

riffle   = halveList ->- zipp ->- unpair
unriffle = pair ->- unzipp ->- append

zipp ([],   [])   = []
zipp (a:as, b:bs) = (a,b) : zipp (as, bs)

unzipp []          = ([],   [])
unzipp ((a,b):abs) = (a:as, b:bs)
  where
    (as, bs) = unzipp abs

pair (x:y:xs) = (x,y) : pair xs
pair xs       = []

unpair ((x,y):xys) = x : y : unpair xys
unpair []          = []

halveList inps = (left,right)
  where
    left  = take half inps
    right = drop half inps
    half  = length inps `div` 2

append (a,b) = a ++ b

----------------------------------------------------------------
-- Connection Patterns

serial circ1 circ2 = circ2 . circ1
circ1 ->- circ2    = serial circ1 circ2

compose []           = id
compose (circ:circs) = circ ->- compose circs

composeN n circ = compose (replicate n circ)

par circ1 circ2 (a, b) = (circ1 a, circ2 b)
circ1 -|- circ2        = par circ1 circ2

parl circ1 circ2 = halveList ->- (circ1 -|- circ2) ->- append


two circ = parl circ circ
ilv circ = unriffle ->- two circ ->- riffle

iter 0 comb circ = circ
iter n comb circ = comb (iter (n-1) comb circ)

twoN n circ = iter n two circ
ilvN n circ = iter n ilv circ

bfly 0 circ = id
bfly n circ = ilv (bfly (n-1) circ) ->- twoN (n-1) circ

pmap circ = pair ->- map circ ->- unpair

tri circ []         = []
tri circ (inp:inps) = inp : (map circ ->- tri circ) inps

mirror circ (a, b) = (c, d)
  where
    (d, c) = circ (b, a)

row circ (carryIn, [])   = ([], carryIn)
row circ (carryIn, a:as) = (b:bs, carryOut)
  where
    (b, carry)     = circ (carryIn, a)
    (bs, carryOut) = row circ (carry, as)

column circ = mirror (row (mirror circ))

grid circ = row (column circ)

----------------------------------------------------------------
-- the end.

