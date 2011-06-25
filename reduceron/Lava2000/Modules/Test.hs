module Test where

import Signal
import Sequential
import Generic

import LavaRandom
  ( newRnd
  )

----------------------------------------------------------------
-- test

test :: (Constructive a, Show b, Generic b) => (a -> b) -> IO [b]
test circ =
  do rnd <- newRnd
     let res = simulateSeq (\_ -> circ (random rnd)) (replicate 100 ())
     print res
     return res

----------------------------------------------------------------
-- the end.

