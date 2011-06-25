module Red7.Generate (genRed7) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red7.ProcState
import Red7.Reduceron

genRed7 :: String -> [Int] -> IO ()
genRed7 str code = writeVhdlInputOutput str (circ code) () outs
  where
    outs = (varList 6 "status", varList 8 "result")

circ         :: [Int] -> () -> ([Bit], Word)
circ code () =  snd (rtl (fde True high low) m)
  where
    m =  do s <- procState code
            reduceron s
            result <- readVar (r0 s)
            hlt <- readVar (halted s)
            return (hlt ++ replicate 5 low, take 8 result)
