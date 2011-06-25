module Red16.ProcState where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL2
import CircLib.CascadedRam
import CircLib.BlockRam
import CircLib.Common
import Red16.Widths
import Red16.Mem
import Red16.Bytecode

statusHalted   =  [high, low, low, low, low, low]
statusUnwind   =  [low, high, low, low, low, low]
statusUnfold   =  [low, low, high, low, low, low]
statusInt      =  [low, low, low, high, low, low]
statusPrim     =  [low, low, low, low, high, low]
statusGc       =  [low, low, low, low, low, high]

data GCState =
  GC {
    gcsp :: Var
  , gchp :: Var
  , apToCopy :: Var
  , apPtr :: Var
  , newApAddr :: Var
  , wasEnd :: Var
  , collected :: Var
  , gcHeap :: (Mem, Mem)
  }

newGC =
  do gcspReg <- newReg stkAddrWidth
     gchpReg <- newReg addrWidth
     apToCopyReg <- newReg addrWidth
     apPtrReg <- newReg addrWidth
     newApAddrReg <- newReg addrWidth
     wasEndReg <- newReg 1
     collectedReg <- newReg addrWidth
     gcHeapMem <- createMem True (addrWidth) (dualWide12k []) init

     return $ GC {
                gcsp = gcspReg
              , gchp = gchpReg
              , apToCopy = apToCopyReg
              , apPtr = apPtrReg
              , newApAddr = newApAddrReg
              , wasEnd = wasEndReg
              , collected = collectedReg
              , gcHeap = gcHeapMem
              }

data ProcState =  PS { sp         :: Var
                     , hp         :: Var
                     , cp         :: Var
                     , top        :: Var
                     , base       :: Var
                     , stackBuf   :: [Word]
                     , stackBufEn :: Var
                     , root       :: Var
                     , status     :: Var
                     , wordsTodo  :: Var
                     , aluArith   :: Var
                     , aluRes     :: Var
                     , sp2        :: Var
                     , spPlus4    :: Word
                     , gc         :: GCState
                     , heap       :: (Mem, Mem)
                     , stack      :: (Mem, Mem)
                     , astack     :: (Mem, Mem)
                     , code       :: (Mem, Mem)
                     }

procState      :: [Int] -> RTL ProcState
procState code =
  do spReg     <- newReg stkAddrWidth
     hpReg     <- newReg addrWidth
     cpReg     <- newReg combAddrWidth
     statusReg <- newReg 6
     topReg    <- newReg wordSize
     sp2Reg    <- newReg stkAddrWidth
     aluResReg <- newReg wordSize
     aluArithReg <- newReg 1
     wordsTodoReg <- newReg 10
     rootReg <- newReg wordSize
     baseReg <- newReg addrWidth
     gcObject <- newGC

     heapMem <- createMem True addrWidth (dualWide32k []) id
     (stackARaw, stackBRaw) <-
       createMem False stkAddrWidth (dualWide4k []) id
     astackMem <- createMem False stkAddrWidth (dualWide4k []) id
     codeMem <- createMem True combAddrWidth (dualWide4k code) id

     stackEnSig <- newSig 1
     let [stackEnVal] = stackEnSig!val

     let stackMem = (bufferMem stackARaw, bufferMem stackBRaw)
     let stkBuf = groupN wordSize $ map (fde False stackEnVal)
                                  $ memDatIn stackARaw ++ memDatIn stackBRaw

     spNew <- readNew (variable spReg)

     return $ PS { sp         = spReg
                 , hp         = hpReg
                 , cp         = cpReg
                 , status     = statusReg
                 , top        = topReg
                 , stackBuf   = stkBuf
                 , sp2        = sp2Reg
                 , aluArith   = aluArithReg
                 , aluRes     = aluResReg
                 , wordsTodo  = wordsTodoReg
                 , root       = rootReg
                 , base       = baseReg
                 , spPlus4    = map (fde False high) (spNew /+/ unsigned 4)
                 , heap       = heapMem
                 , stack      = stackMem
                 , astack     = astackMem
                 , code       = codeMem
                 , gc         = gcObject
                 , stackBufEn = stackEnSig
                 }
