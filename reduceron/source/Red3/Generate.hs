module Red3.Generate (genRed3) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red3.ProcState
import Red3.Reduceron

genRed3 :: String -> [Int] -> IO ()
genRed3 str code = writeVhdlInputOutput str (circ code) () outs
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
