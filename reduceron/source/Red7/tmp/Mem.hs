module Red7.Mem where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import Red7.ProcState

readMem                 :: ProcState -> Word -> RTL Word
readMem s loc           =  do writeVar (addr s) loc
                              return (datin s)

writeMem                :: ProcState -> Word -> Word -> RTL ()
writeMem s v a          =  do writeVar (addr s) a
                              writeVar (we s) [high]
                              writeVar (datout s) v

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

readGc                  :: ProcState -> Word -> RTL Word
readGc s loc            =  do writeVar (gcAddr s) loc
                              return (gcDatin s)

writeGc                 :: ProcState -> Word -> Word -> RTL ()
writeGc s v a           =  do writeVar (gcAddr s) a
                              writeVar (gcWe s) [high]
                              writeVar (gcDatout s) v

readGcOff               :: ProcState -> (OffsetReg, Int) -> RTL Word
readGcOff s (r, o)      =  do loc <- setOffset r o
                              readGc s loc

writeGcOff              :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeGcOff s v (r, o)   =  do loc <- setOffset r o
                              writeGc s v loc

readGcOff'              :: ProcState -> (OffsetReg, Int) -> RTL Word
readGcOff' s (r, o)     =  do loc   <-  setOffset r o
                              reg r <-- loc
                              readGc s loc

writeGcOff'             :: ProcState -> Word -> (OffsetReg, Int) ->RTL ()
writeGcOff' s v (r, o)  =  do loc   <- setOffset r o
                              reg r <-- loc
                              writeGc s v loc
