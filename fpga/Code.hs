module Code
  ( Code       -- data Code n m
  , newCode    -- :: [Integer] -> Word n -> RamAlgorithm -> New (Code m)
  ) where

-- A value of type 'Code n m' is a read-only random-access memory
-- storing '2^n' elements elements of type 'Word m'.

import Prelude hiding (Word, (.))
import Lava
import Recipe

type Code m = Word m

newCode :: (N n, N m) => [Integer] -> Word n -> RamAlgorithm -> New (Code m)
newCode init address ramAlgorithm = return (ram init "code_" ramAlgorithm ramIns)
  where
     ramIns = RamInputs {
                ramData    = 0
              , ramAddress = address
              , ramWrite   = low
              }
