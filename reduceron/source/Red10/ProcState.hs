module Red10.ProcState where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import CircLib.CascadedRam
import CircLib.BlockRam
import Red10.Mem

wordSize       :: Int
wordSize       =  18

addrWidth      :: Int
addrWidth      =  15

stkAddrWidth   :: Int
stkAddrWidth   =  12

combAddrWidth  :: Int
combAddrWidth  =  12

maxAddr        :: Word
maxAddr        =  replicate addrWidth high

maxStkAddr     :: Word
maxStkAddr     =  replicate stkAddrWidth high

word i         =  i `ofWidth` wordSize

address i      =  i `ofWidth` addrWidth

stkAddress i   =  i `ofWidth` stkAddrWidth

combAddress i  =  i `ofWidth` combAddrWidth

statusHalted   =  [high, low, low, low, low, low]
statusUnwind   =  [low, high, low, low, low, low]
statusUnfold   =  [low, low, high, low, low, low]
statusInt      =  [low, low, low, high, low, low]
statusPrim     =  [low, low, low, low, high, low]
statusGc       =  [low, low, low, low, low, high]

data ProcState =  PS { sp         :: OffsetReg
                     , hp         :: OffsetReg
                     , gchp       :: Var
                     , status     :: Var
                     , top        :: Var
                     , topIsStale :: Var
                     , top8       :: [Var]
                     , r0         :: Var
                     , r1         :: Var
                     , r2         :: Var
                     , a0         :: Var
                     , a1         :: Var
                     , a2         :: Var
                     , a3         :: Var
                     , sa0        :: Var
                     , ca0        :: Var
                     , nib0       :: Var
                     , nib1       :: Var
                     , dec0       :: Var
                     , bit0       :: Var
                     , bit1       :: Var
                     , heapA      :: Mem
                     , stackA     :: Mem
                     , astackA    :: Mem
                     , gcSpaceA   :: Mem
                     , codeA      :: Mem
                     , heapB      :: Mem
                     , stackB     :: Mem
                     , astackB    :: Mem
                     , gcSpaceB   :: Mem
                     , codeB      :: Mem
                     }

procState      :: [Int] -> RTL ProcState
procState code =
  do spReg     <- newOffsetReg stkAddrWidth 4
     hpReg     <- newOffsetReg addrWidth 4
     gchpReg   <- newReg addrWidth
     statusReg <- newReg 6
     topReg    <- newReg wordSize
     stale     <- newReg 1
     top8Regs  <- sequence (replicate 8 (newReg wordSize))
     r0Reg     <- newReg wordSize
     r1Reg     <- newReg wordSize
     r2Reg     <- newReg wordSize
     a0Reg     <- newReg addrWidth
     a1Reg     <- newReg addrWidth
     a2Reg     <- newReg addrWidth
     a3Reg     <- newReg addrWidth
     sa0Reg    <- newReg stkAddrWidth
     ca0Reg    <- newReg combAddrWidth
     nib0Reg   <- newReg 4
     nib1Reg   <- newReg 4
     dec0Reg   <- newReg 10
     bit0Reg   <- newReg 1
     bit1Reg   <- newReg 1

     (heapAMem, heapBMem) <-
       createMem True addrWidth (dualWide32k []) id
     (gcSpaceAMem, gcSpaceBMem) <-
       createMem True (addrWidth) (dualWide12k []) init
     (stackAMem, stackBMem) <-
       createMem False stkAddrWidth (dualWide4k []) id
     (astackAMem, astackBMem) <-
       createMem False stkAddrWidth (dualWide4k []) id
     --(codeAMem, codeBMem) <-
     --  createMem False combAddrWidth (dualWide4k code) id
     (codeAMem, codeBMem) <-
       createMem True combAddrWidth (dualWide4k code) id
     
     return $ PS { sp         = spReg
                 , hp         = hpReg
                 , gchp       = gchpReg
                 , status     = statusReg
                 , top        = topReg
                 , topIsStale = stale
                 , top8       = top8Regs
                 , r0         = r0Reg
                 , r1         = r1Reg
                 , r2         = r2Reg
                 , a0         = a0Reg
                 , a1         = a1Reg
                 , a2         = a2Reg
                 , a3         = a3Reg
                 , sa0        = sa0Reg
                 , ca0        = ca0Reg
                 , bit0       = bit0Reg
                 , bit1       = bit1Reg
                 , nib0       = nib0Reg
                 , nib1       = nib1Reg
                 , dec0       = dec0Reg

                 , heapA      = heapAMem
                 , stackA     = stackAMem
                 , astackA    = astackAMem
                 , gcSpaceA   = gcSpaceAMem
                 , codeA      = codeAMem
                 , heapB      = heapBMem
                 , stackB     = stackBMem
                 , astackB    = astackBMem
                 , gcSpaceB   = gcSpaceBMem
                 , codeB      = codeBMem
                 }
