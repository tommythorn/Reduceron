module Red6.Generate (genRed6) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import Red6.ProcState
import Red6.Reduceron

genRed6 :: String -> [Int] -> IO ()
genRed6 str code = writeVhdlInputOutput str (circ code) () outs
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
