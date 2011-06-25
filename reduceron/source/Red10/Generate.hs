module Red10.Generate (genRed10) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red10.ProcState
import Red10.Reduceron

genRed10 :: String -> [Int] -> IO ()
genRed10 str code = writeVhdlInputOutput str (circ code) () outs
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
