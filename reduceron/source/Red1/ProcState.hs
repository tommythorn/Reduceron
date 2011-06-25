module Red1.ProcState where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import CircLib.CascadedRam

wordSize       :: Int
wordSize       =  18

addrWidth      :: Int
addrWidth      =  15

maxAddr        :: Word
maxAddr        =  replicate addrWidth high

word i         =  i `ofWidth` wordSize

address i      =  i `ofWidth` addrWidth

data ProcState =  PS { sp       :: OffsetReg
                     , hp       :: OffsetReg
                     , halted   :: Var
                     , top      :: Var
                     , r0       :: Var
                     , r1       :: Var
                     , a0       :: Var
                     , a1       :: Var
                     , nib0     :: Var
                     , nib1     :: Var
                     , dec0     :: Var
                     , bit0     :: Var
                     , addr     :: Var
                     , datin    :: Word
                     , datout   :: Var
                     , we       :: Var
                     , hpInit   :: Int
                     }

procState      :: [Int] -> RTL ProcState
procState code =  do spReg      <- newOffsetReg addrWidth 4
                     hpReg      <- newOffsetReg addrWidth 4
                     haltedReg  <- newReg 1
                     topReg     <- newReg wordSize
                     r0Reg      <- newReg wordSize
                     r1Reg      <- newReg wordSize
                     a0Reg      <- newReg addrWidth
                     a1Reg      <- newReg addrWidth
                     nib0Reg    <- newReg 4
                     nib1Reg    <- newReg 4
                     dec0Reg    <- newReg 10
                     bit0Reg    <- newReg 1
                     addrSig    <- newSig addrWidth
                     datoutSig  <- newSig wordSize
                     weSig      <- newSig 1

                     addrVal    <- readVar addrSig
                     datoutVal  <- readVar datoutSig
                     weVal      <- readVar weSig

                     let memInps = Mem_Inps {
                                       mem_addr  = addrVal
                                     , mem_datIn = datoutVal
                                     , mem_we    = head weVal
                                   }

                     let memOut = delay (replicate 18 low)
                                        (mem32k code memInps)

                     return $ PS { sp      = spReg
                                 , hp      = hpReg
                                 , halted  = haltedReg
                                 , top     = topReg
                                 , r0      = r0Reg
                                 , r1      = r1Reg
                                 , a0      = a0Reg
                                 , a1      = a1Reg
                                 , bit0    = bit0Reg
                                 , nib0    = nib0Reg
                                 , nib1    = nib1Reg
                                 , dec0    = dec0Reg
                                 , addr    = addrSig
                                 , datin   = memOut
                                 , datout  = datoutSig
                                 , we      = weSig
                                 , hpInit  = length code + 1
                                 }
