module Red6.ProcState where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import CircLib.CascadedRam
import CircLib.BlockRam

wordSize       :: Int
wordSize       =  18

addrWidth      :: Int
addrWidth      =  15

stkAddrWidth   :: Int
stkAddrWidth   =  10

combAddrWidth  :: Int
combAddrWidth  =  10

maxAddr        :: Word
maxAddr        =  replicate addrWidth high

maxStkAddr     :: Word
maxStkAddr     =  replicate stkAddrWidth high

word i         =  i `ofWidth` wordSize

address i      =  i `ofWidth` addrWidth

stkAddress i   =  i `ofWidth` stkAddrWidth

data ProcState =  PS { sp         :: OffsetReg
                     , hp         :: OffsetReg
                     , gchp       :: Var
                     , halted     :: Var
                     , top        :: Var
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
                     , addr       :: Var
                     , datin      :: Word
                     , datout     :: Var
                     , we         :: Var
                     , gcAddr     :: Var
                     , gcDatin    :: Word
                     , gcDatout   :: Var
                     , gcWe       :: Var
                     , combAddr   :: Var
                     , combDatin  :: Word
                     , combDatout :: Var
                     , combWe     :: Var
                     , stkAddr    :: Var
                     , stkDatin   :: Word
                     , stkDatout  :: Var
                     , stkWe      :: Var
                     , astkAddr   :: Var
                     , astkDatin  :: Word
                     , astkDatout :: Var
                     , astkWe     :: Var
                     }

procState      :: [Int] -> RTL ProcState
procState code =  do spReg         <- newOffsetReg stkAddrWidth 4
                     hpReg         <- newOffsetReg addrWidth 4
                     gchpReg       <- newReg addrWidth
                     haltedReg     <- newReg 1
                     topReg        <- newReg wordSize
                     r0Reg         <- newReg wordSize
                     r1Reg         <- newReg wordSize
                     r2Reg         <- newReg wordSize
                     a0Reg         <- newReg addrWidth
                     a1Reg         <- newReg addrWidth
                     a2Reg         <- newReg addrWidth
                     a3Reg         <- newReg addrWidth
                     sa0Reg        <- newReg stkAddrWidth
                     ca0Reg        <- newReg combAddrWidth
                     nib0Reg       <- newReg 4
                     nib1Reg       <- newReg 4
                     dec0Reg       <- newReg 10
                     bit0Reg       <- newReg 1
                     bit1Reg       <- newReg 1
                     addrSig       <- newSig addrWidth
                     datoutSig     <- newSig wordSize
                     weSig         <- newSig 1
                     gcAddrSig     <- newSig addrWidth
                     gcDatoutSig   <- newSig wordSize
                     gcWeSig       <- newSig 1
                     combAddrSig   <- newSig combAddrWidth
                     combDatoutSig <- newSig wordSize
                     combWeSig     <- newSig 1
                     stkAddrSig    <- newSig stkAddrWidth
                     stkDatoutSig  <- newSig wordSize
                     stkWeSig      <- newSig 1
                     astkAddrSig   <- newSig stkAddrWidth
                     astkDatoutSig <- newSig wordSize
                     astkWeSig     <- newSig 1



                     addrVal       <- readVar addrSig
                     datoutVal     <- readVar datoutSig
                     weVal         <- readVar weSig
                     gcAddrVal     <- readVar gcAddrSig
                     gcDatoutVal   <- readVar gcDatoutSig
                     gcWeVal       <- readVar gcWeSig
                     combAddrVal   <- readVar combAddrSig
                     combDatoutVal <- readVar combDatoutSig
                     combWeVal     <- readVar combWeSig
                     stkAddrVal    <- readVar stkAddrSig
                     stkDatoutVal  <- readVar stkDatoutSig
                     stkWeVal      <- readVar stkWeSig
                     astkAddrVal   <- readVar astkAddrSig
                     astkDatoutVal <- readVar astkDatoutSig
                     astkWeVal     <- readVar astkWeSig

                     let memInps = Mem_Inps {
                                       mem_addr  = addrVal
                                     , mem_datIn = datoutVal
                                     , mem_we    = head weVal
                                   }

                     let memOut = delay (replicate wordSize low)
                                        (mem32k [] memInps)

                     let gcMemInps = Mem_Inps {
                                         mem_addr  = init gcAddrVal
                                       , mem_datIn = gcDatoutVal
                                       , mem_we    = head gcWeVal
                                     }

                     let gcMemOut = delay (replicate wordSize low)
                                          (mem16k [] gcMemInps)

                     let combInps = BRam18_Inps {
                                        bram18_addr  = combAddrVal
                                      , bram18_datIn = combDatoutVal
                                      , bram18_we    = head combWeVal
                                    }

                     let combOut = bram18 code combInps

                     let stkInps = BRam18_Inps {
                                       bram18_addr  = stkAddrVal
                                     , bram18_datIn = stkDatoutVal
                                     , bram18_we    = head stkWeVal
                                   }

                     let stkOut = bram18 [] stkInps

                     let astkInps = BRam18_Inps {
                                        bram18_addr  = astkAddrVal
                                      , bram18_datIn = astkDatoutVal
                                      , bram18_we    = head astkWeVal
                                    }

                     let astkOut = bram18 [] astkInps

                     return $ PS { sp         = spReg
                                 , hp         = hpReg
                                 , gchp       = gchpReg
                                 , halted     = haltedReg
                                 , top        = topReg
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
                                 , addr       = addrSig
                                 , datin      = memOut
                                 , datout     = datoutSig
                                 , we         = weSig
                                 , gcAddr     = gcAddrSig
                                 , gcDatin    = gcMemOut
                                 , gcDatout   = gcDatoutSig
                                 , gcWe       = gcWeSig
                                 , combAddr   = combAddrSig
                                 , combDatin  = combOut
                                 , combDatout = combDatoutSig
                                 , combWe     = combWeSig
                                 , stkAddr    = stkAddrSig
                                 , stkDatin   = stkOut
                                 , stkDatout  = stkDatoutSig
                                 , stkWe      = stkWeSig
                                 , astkAddr   = astkAddrSig
                                 , astkDatin  = astkOut
                                 , astkDatout = astkDatoutSig
                                 , astkWe     = astkWeSig
                                 }
