module Red1.Mem where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import Red1.ProcState

readMem                 :: ProcState -> Word -> RTL Word
readMem s loc           =  do writeVar (addr s) loc
                              tick
                              tick
                              return (datin s)

writeMem                :: ProcState -> Word -> Word -> RTL ()
writeMem s v a          =  do writeVar (addr s) a
                              writeVar (we s) [high]
                              writeVar (datout s) v
                              tick

readMemOff              :: ProcState -> (OffsetReg, Int) -> RTL Word
readMemOff s (r, o)     =  do loc <- setOffset r o
                              readMem s loc

writeMemOff             :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeMemOff s v (r, o)  =  do loc <- setOffset r o
                              writeMem s v loc

readMemOff'             :: ProcState -> (OffsetReg, Int) -> RTL Word
readMemOff' s (r, o)    =  do loc   <-  setOffset r o
                              reg r <-- loc
                              readMem s loc

writeMemOff'            :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeMemOff' s v (r, o) =  do loc   <- setOffset r o
                              reg r <-- loc
                              writeMem s v loc
