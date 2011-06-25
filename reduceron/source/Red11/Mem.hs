module Red11.Mem where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import CircLib.CascadedRam
import CircLib.Common
import Red11.Widths

data Mem = Mem { memAddr    :: Var
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
     
     valAddr1   <- readVar sigAddr1
     valDatOut1 <- readVar sigDatOut1
     valWE1     <- readVar sigWE1

     valAddr2   <- readVar sigAddr2
     valDatOut2 <- readVar sigDatOut2
     valWE2     <- readVar sigWE2

     let (out1, out2) = mem ( Wide_Inps {
                                wide_addr  = f valAddr1
                              , wide_datIn = valDatOut1
                              , wide_we    = valWE1
                              }
                            , Wide_Inps {
                                wide_addr  = f valAddr2
                              , wide_datIn = valDatOut2
                              , wide_we    = valWE2
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

quadRead :: Mem -> Word -> RTL [Word]
quadRead mem addr =
  do writeVar (memAddr mem) addr
     return (groupN 18 (memDatIn mem))

quadWrite :: Mem -> [Word] -> [Bit] -> Word -> RTL ()
quadWrite mem val mask addr =
  do writeVar (memAddr mem) addr
     writeVar (memWE mem) mask
     writeVar (memDatOut mem) (concat val)

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
     valB <- quadRead memB (addr /+/ (4 `ofWidth` length addr))
     return (valA ++ valB)

octoWrite :: (Mem, Mem) -> [Word] -> Word -> RTL ()
octoWrite (memA, memB) ws addr =
  do quadWrite memA (take 4 ws) mask addr
     quadWrite memB (drop 4 ws) mask (addr /+/ (4 `ofWidth` length addr))
  where
    mask = [high, high, high, high]

--

unitReadOff :: Mem -> (OffsetReg, Int) -> RTL Word
unitReadOff mem (r, o) =
  do addr <- setOffset r o
     unitRead mem addr

unitReadOff' :: Mem -> (OffsetReg, Int) -> RTL Word
unitReadOff' mem (r, o) = 
  do addr <- setOffset r o
     reg r <-- addr
     unitRead mem addr

unitWriteOff :: Mem -> Word -> (OffsetReg, Int) -> RTL ()
unitWriteOff mem v (r, o) =
  do addr <- setOffset r o
     unitWrite mem v addr

unitWriteOff' :: Mem -> Word -> (OffsetReg, Int) -> RTL ()
unitWriteOff' mem v (r, o) =
  do addr <- setOffset r o
     reg r <-- addr
     unitWrite mem v addr
