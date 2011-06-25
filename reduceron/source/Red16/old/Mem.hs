module Red16.Mem where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL2
import CircLib.CascadedRam
import CircLib.Common
import Red16.Widths

data Mem =
  Mem { memAddr    :: Var
      , memDatIn   :: Word
      , memDatOut  :: Var
      , memWE      :: Var
      }

createMem :: Bool                               -- Pipelined reads?
          -> Int                                -- Address width
          -> (DualWide_Inps -> ([Bit], [Bit]))  -- Memory function
          -> ([Bit] -> [Bit])                   -- Apply function to address
          -> RTL (Mem, Mem)
createMem pipeline addrWidth mem f =
  do sigAddr1   <- newSig addrWidth
     sigDatOut1 <- newSig (wordSize * 4)
     sigWE1     <- newSig 4
     sigAddr2   <- newSig addrWidth
     sigDatOut2 <- newSig (wordSize * 4)
     sigWE2     <- newSig 4
     
     let (out1, out2) = mem ( Wide_Inps {
                                wide_addr  = f (sigAddr1!val)
                              , wide_datIn = sigDatOut1!val
                              , wide_we    = sigWE1!val
                              }
                            , Wide_Inps {
                                wide_addr  = f (sigAddr2!val)
                              , wide_datIn = sigDatOut2!val
                              , wide_we    = sigWE2!val
                              }
                            )

     let out1' = if   pipeline
                 then delay (replicate (4*wordSize) low) out1
                 else out1

     let out2' = if   pipeline
                 then delay (replicate (4*wordSize) low) out2
                 else out2

     return ( Mem {
                memAddr   = sigAddr1
              , memDatIn  = out1'
              , memDatOut = sigDatOut1
              , memWE     = sigWE1
              }
            , Mem {
                 memAddr   = sigAddr2
              , memDatIn  = out2'
              , memDatOut = sigDatOut2
              , memWE     = sigWE2
             }
            )

bufferMem :: Mem -> Mem
bufferMem mem =
  mem { memDatIn = delay (replicate (4*wordSize) low) (memDatIn mem) }

quadRead :: Mem -> Word -> RTL [Word]
quadRead mem addr =
  do memAddr mem <== addr
     return (groupN 18 (memDatIn mem))

quadWrite :: Mem -> [Word] -> [Bit] -> Word -> RTL ()
quadWrite mem val mask addr =
  do (memAddr mem) <== addr
     (memWE mem) <== mask
     (memDatOut mem) <== (concat val)

unitRead :: Mem -> Word -> RTL Word
unitRead mem addr =
  do val <- quadRead mem addr
     return (head val)

unitWrite :: Mem -> Word -> Word -> RTL ()
unitWrite mem val addr = quadWrite mem [val,z,z,z] mask addr
  where
    mask = [high, low, low, low]
    z = replicate (length val) low

octoRead :: (Mem, Mem) -> Word -> RTL [Word]
octoRead (memA, memB) addr =
  do valA <- quadRead memA addr
     valB <- quadRead memB (addr /+/ unsigned 4)
     return (valA ++ valB)

octoWrite :: (Mem, Mem) -> [Word] -> Word -> RTL ()
octoWrite (memA, memB) ws addr =
  do quadWrite memA (take 4 ws) mask addr
     quadWrite memB (drop 4 ws) mask (addr /+/ unsigned 4)
  where
    mask = [high, high, high, high]

octoOut (memA, memB) = groupN wordSize (valA ++ valB)
  where
    valA = memA!memDatIn
    valB = memB!memDatIn

unitOut (memA, memB) = take wordSize (memA!memDatIn)
