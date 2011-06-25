module LavaRandom
  ( Rnd
  , newRnd
  , next
  , split
  )
 where

import Random
  ( StdGen
  , newStdGen
  , next
  , split
  )

type Rnd = StdGen

newRnd = newStdGen
