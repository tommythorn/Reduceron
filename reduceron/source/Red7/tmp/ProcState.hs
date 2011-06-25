module Red7.ProcState where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import CircLib.CascadedRam
import CircLib.Common
import Red7.Bytecode

wordSize       :: Int
wordSize       =  18

addrWidth      :: Int
addrWidth      =  15

maxAddr        :: Word
maxAddr        =  replicate addrWidth high

astkBase       :: Word
astkBase       =  address (2^addrWidth - 1 - 2^10)

word i         =  i `ofWidth` wordSize

address i      =  i `ofWidth` addrWidth

trueAddr = mkFunNode (word 2)

falseAddr = mkFunNode (word 4)

data ProcState =  PS { sp       :: OffsetReg
                     , hp       :: OffsetReg
                     , asp      :: Word
                     , gchp     :: Var
                     , halted   :: Var
                     , top      :: Var
                     , r0       :: Var
                     , r1       :: Var
                     , r2       :: Var
                     , a0       :: Var
                     , a1       :: Var
                     , a2       :: Var
                     , a3       :: Var
                     , sa0      :: Var
                     , nib0     :: Var
                     , nib1     :: Var
                     , dec0     :: Var
                     , bit0     :: Var
                     , bit1     :: Var
                     , bit2     :: Var
                     , addr     :: Var
                     , datin    :: Word
                     , datout   :: Var
                     , we       :: Var
                     , hpInit   :: Int
                     , gcAddr   :: Var
                     , gcDatin  :: Word
                     , gcDatout :: Var
                     , gcWe     :: Var
                     }

procState      :: [Int] -> RTL ProcState
procState code =  do spReg      <- newOffsetReg addrWidth 4
                     hpReg      <- newOffsetReg addrWidth 4
                     gchpReg    <- newReg addrWidth
                     haltedReg  <- newReg 1
                     topReg     <- newReg wordSize
                     r0Reg      <- newReg wordSize
                     r1Reg      <- newReg wordSize
                     r2Reg      <- newReg wordSize
                     a0Reg      <- newReg addrWidth
                     a1Reg      <- newReg addrWidth
                     a2Reg      <- newReg addrWidth
                     a3Reg      <- newReg addrWidth
                     sa0Reg     <- newReg addrWidth
                     nib0Reg    <- newReg 4
                     nib1Reg    <- newReg 4
                     dec0Reg    <- newReg 10
                     bit0Reg    <- newReg 1
                     bit1Reg    <- newReg 1
                     bit2Reg    <- newReg 1
                     addrSig    <- newSig addrWidth
                     datoutSig  <- newSig wordSize
                     weSig      <- newSig 1
                     gcAddrSig   <- newSig addrWidth
                     gcDatoutSig <- newSig wordSize
                     gcWeSig     <- newSig 1

                     spVal      <- readVar (reg spReg)
                     addrVal    <- readVar addrSig
                     datoutVal  <- readVar datoutSig
                     weVal      <- readVar weSig
                     gcAddrVal   <- readVar gcAddrSig
                     gcDatoutVal <- readVar gcDatoutSig
                     gcWeVal     <- readVar gcWeSig

                     let memInps = Mem_Inps {
                                       mem_addr  = addrVal
                                     , mem_datIn = datoutVal
                                     , mem_we    = head weVal
                                   }

                     let memOut = delay (replicate 18 low)
                                        (mem32k code memInps)

                     let gcMemInps = Mem_Inps {
                                         mem_addr  = init gcAddrVal
                                       , mem_datIn = gcDatoutVal
                                       , mem_we    = head gcWeVal
                                     }

                     let gcMemOut = delay (replicate wordSize low)
                                          (mem16k [] gcMemInps)

                     return $ PS { sp      = spReg
                                 , asp     = spVal /-/ address (2^10)
                                 , hp      = hpReg
                                 , gchp    = gchpReg
                                 , halted  = haltedReg
                                 , top     = topReg
                                 , r0      = r0Reg
                                 , r1      = r1Reg
                                 , r2      = r2Reg
                                 , a0      = a0Reg
                                 , a1      = a1Reg
                                 , a2      = a2Reg
                                 , a3      = a3Reg
                                 , sa0     = sa0Reg
                                 , bit0    = bit0Reg
                                 , bit1    = bit1Reg
                                 , bit2    = bit2Reg
                                 , nib0    = nib0Reg
                                 , nib1    = nib1Reg
                                 , dec0    = dec0Reg
                                 , addr    = addrSig
                                 , datin   = memOut
                                 , datout  = datoutSig
                                 , we      = weSig
                                 , hpInit  = length code + 1
                                 , gcAddr   = gcAddrSig
                                 , gcDatin  = gcMemOut
                                 , gcDatout = gcDatoutSig
                                 , gcWe     = gcWeSig
                                 }
