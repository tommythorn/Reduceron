module Red6.Mem where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import Red6.ProcState

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

writeGcOff'             :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeGcOff' s v (r, o)  =  do loc   <- setOffset r o
                              reg r <-- loc
                              writeGc s v loc

readComb                :: ProcState -> Word -> RTL Word
readComb s loc          =  do writeVar (combAddr s) loc
                              return (combDatin s)

readStk                 :: ProcState -> Word -> RTL Word
readStk s loc           =  do writeVar (stkAddr s) loc
                              return (stkDatin s)

writeStk                :: ProcState -> Word -> Word -> RTL ()
writeStk s v a          =  do writeVar (stkAddr s) a
                              writeVar (stkWe s) [high]
                              writeVar (stkDatout s) v

readStkOff              :: ProcState -> (OffsetReg, Int) -> RTL Word
readStkOff s (r, o)     =  do loc <- setOffset r o
                              readStk s loc

writeStkOff             :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeStkOff s v (r, o)  =  do loc <- setOffset r o
                              writeStk s v loc

readStkOff'             :: ProcState -> (OffsetReg, Int) -> RTL Word
readStkOff' s (r, o)    =  do loc   <-  setOffset r o
                              reg r <-- loc
                              readStk s loc

writeStkOff'            :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeStkOff' s v (r, o) =  do loc   <- setOffset r o
                              reg r <-- loc
                              writeStk s v loc

readAStk                :: ProcState -> Word -> RTL Word
readAStk s loc          =  do writeVar (astkAddr s) loc
                              return (astkDatin s)

readAStkOff             :: ProcState -> (OffsetReg, Int) -> RTL Word
readAStkOff s (r, o)    =  do loc <- setOffset r o
                              readAStk s loc

readAStkOff'            :: ProcState -> (OffsetReg, Int) -> RTL Word
readAStkOff' s (r, o)   =  do loc   <-  setOffset r o
                              reg r <-- loc
                              readAStk s loc

writeAStk               :: ProcState -> Word -> Word -> RTL ()
writeAStk s v a         =  do writeVar (astkAddr s) a
                              writeVar (astkWe s) [high]
                              writeVar (astkDatout s) v

writeAStkOff            :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeAStkOff s v (r, o) =  do loc <- setOffset r o
                              writeAStk s v loc
