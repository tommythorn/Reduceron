module Red11.Generate (genRed11) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red11.ProcState
import Red11.Reduceron

genRed11 :: String -> [Int] -> IO ()
genRed11 str code = writeVhdlInputOutput str (circ code) () outs
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
