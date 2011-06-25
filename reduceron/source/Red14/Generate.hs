module Red14.Generate (genRed14) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red14.ProcState
import Red14.Reduceron

genRed14 :: String -> [Int] -> IO ()
genRed14 str code = writeVhdlInputOutput str (circ code) () outs
  where
    outs = (varList 6 "status", varList 8 "result")

circ :: [Int] -> () -> (Word, Word)
circ code () =  snd (rtl (fde True high low) m)
  where
    m =  do s <- procState code
            reduceron s
            result <- readVar (r0 s)
            st <- readVar (status s)
            return (st, take 8 result)
