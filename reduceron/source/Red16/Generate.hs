module Red16.Generate (genRed16) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL2
import CircLib.BlockRam
import CircLib.Common
import Red16.ProcState
import Red16.Reduceron

genRed16 :: String -> [Int] -> IO ()
genRed16 str code = writeVhdlInputOutput str (circ code) () outs
  where
    outs = (varList 6 "status", varList 8 "result")

circ :: [Int] -> () -> (Word, Word)
circ code () =  snd (rtl (fde True high low) m)
  where
    m =  do s <- procState code
            reduceron s
            --result <- readVar (r0 s)
            --st <- readVar (status s)
            --let result = s!r0!val
            let result = (s!top!val) `shre` 3
            let st = s!status!val
            return (st, take 8 result)
