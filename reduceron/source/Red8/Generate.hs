module Red8.Generate (genRed8) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red8.ProcState
import Red8.Reduceron

genRed8 :: String -> [Int] -> IO ()
genRed8 str code = writeVhdlInputOutput str (circ code) () outs
  where
    outs = (varList 6 "status", varList 8 "result")

circ         :: [Int] -> () -> (Word, Word)
circ code () =  snd (rtl (fde True high low) m)
  where
    m =  do s <- procState code
            reduceron s
            result <- readVar (r0 s)
            st <- readVar (status s)
            return (st, take 8 result)
