module RegFile
  ( RegFile     -- data RegFile n m
  , newRegFile  -- :: n -> New (RegFile n m)
  , elems       -- :: RegFile n m -> Vec n (Word m)
  , assign      -- :: Word n -> Word m -> RegFile n m -> Recipe
  ) where

-- A value of type 'RegFile n m' is a file of 'n' registers of type
-- 'Word m'.  One register can be updated per clock-cycle.

import Prelude hiding (Word, (.))
import Lava
import Recipe

data RegFile n m =
  RegFile {
    writeTrigger :: Sig N1
  , writeAddr    :: Sig n
  , writeData    :: Sig m
  , elems        :: Vec n (Word m)
  }

newRegFile n =
  do writeTriggerSig <- newSig
     writeAddrSig <- newSig
     writeDataSig <- newSig

     return $ RegFile
       { writeTrigger = writeTriggerSig
       , writeAddr    = writeAddrSig
       , writeData    = writeDataSig
       , elems        = regFile n (writeTriggerSig.val.vhead)
                                  (writeAddrSig.val)
                                  (writeDataSig.val)
       }

regFile n en addr dat = vmap reg addr
  where reg hot = delayEn 0 (en <&> hot) dat

assign :: Word n -> Word m -> RegFile n m -> Recipe
assign addr dat s = do s.writeTrigger <== 1
                       s.writeAddr <== addr
                       s.writeData <== dat


-- Example usage

example :: RegFile N8 N4 -> Recipe
example s = do s.assign (oneHot 0) 1
               tick
               s.assign (oneHot 1) 2
               tick
               tick
               tick
               s.assign (oneHot 2) 3
               tick
               tick
               s.assign (oneHot 3) 4
               tick
               s.assign (oneHot 6) 5
               tick

simExample = simRecipe (newRegFile n8) example elems
