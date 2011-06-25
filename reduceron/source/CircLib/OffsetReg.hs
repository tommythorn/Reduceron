module CircLib.OffsetReg where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL

data OffsetReg    =  OffReg { reg  :: Var
                            , offs :: Var
                            , outp :: Word
                            }

newOffsetReg      :: Int -> Int -> RTL OffsetReg
newOffsetReg n m  =  do r  <- newReg n
                        o  <- newSig m
                        
                        vr <- readVar r
                        vo <- readVar o
                        let out = vr /+/ vo

                        return (OffReg r o out)

setOffset         :: OffsetReg -> Int -> RTL Word
setOffset r o     =  do case o of
                          0 -> readVar (reg r)
                          _ -> do let w = width (offs r)
                                  writeVar (offs r) (o `ofWidth` w)
                                  return (outp r)

increment         :: OffsetReg -> Int -> RTL ()
increment r o     =  do ro    <-  setOffset r o
                        reg r <-- ro
