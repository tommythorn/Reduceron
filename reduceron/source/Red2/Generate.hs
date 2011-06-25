module Red2.Generate (genRed2) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red2.ProcState
import Red2.Reduceron

genRed2 :: String -> [Int] -> IO ()
genRed2 str code = writeVhdlInputOutput str (circ code) () outs
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
