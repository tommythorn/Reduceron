module Red15.Generate (genRed15) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red15.ProcState
import Red15.Reduceron

genRed15 :: String -> [Int] -> IO ()
genRed15 str code = writeVhdlInputOutput str (circ code) () outs
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
