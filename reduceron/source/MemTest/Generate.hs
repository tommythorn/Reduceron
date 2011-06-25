module MemTest.Generate (genMemTest) where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.BlockRam
import MemTest.ProcState
import MemTest.Example

genMemTest :: String -> [Int] -> IO ()
genMemTest str code = writeVhdlInputOutput str (circ code) () outs
  where
    outs = (varList 6 "status", varList 8 "result")

circ         :: [Int] -> () -> ([Bit], Word)
circ code () =  snd (rtl (fde True high low) m)
  where
    m =  do s <- procState code
            example s
            result <- readVar (d0 s)
            h <- readVar (haltFlag s)
            return (h++replicate 5 low, take 8 result)
