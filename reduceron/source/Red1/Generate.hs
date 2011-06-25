module Red1.Generate (genRed1) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red1.ProcState
import Red1.Reduceron

genRed1 :: String -> [Int] -> IO ()
genRed1 str code = writeVhdlInputOutput str (circ code) () outs
  where
    outs = (var "halted", varList 8 "result")

circ         :: [Int] -> () -> (Bit, Word)
circ code () =  snd (rtl (fde True high low) m)
  where
    m =  do s <- procState code
            reduceron s
            result <- readVar (r0 s)
            hlt <- readVar (halted s)
            return (head hlt, take 8 result)
