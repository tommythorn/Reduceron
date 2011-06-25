module Red11.Compat where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import Red11.ProcState
import Red11.Mem

readMem :: ProcState -> Word -> RTL Word
readMem s = unitRead (heapA s)

writeMem :: ProcState -> Word -> Word -> RTL ()
writeMem s = unitWrite (heapA s)

readMemOff :: ProcState -> (OffsetReg, Int) -> RTL Word
readMemOff s = unitReadOff (heapA s)

writeMemOff :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeMemOff s = unitWriteOff (heapA s)

readMemOff' :: ProcState -> (OffsetReg, Int) -> RTL Word
readMemOff' s = unitReadOff' (heapA s)

writeMemOff' :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeMemOff' s = unitWriteOff' (heapA s)

readGc :: ProcState -> Word -> RTL Word
readGc s = unitRead (gcSpaceA s)

writeGc :: ProcState -> Word -> Word -> RTL ()
writeGc s = unitWrite (gcSpaceA s)

readGcOff :: ProcState -> (OffsetReg, Int) -> RTL Word
readGcOff s = unitReadOff (gcSpaceA s)

writeGcOff :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeGcOff s = unitWriteOff (gcSpaceA s)

readGcOff' :: ProcState -> (OffsetReg, Int) -> RTL Word
readGcOff' s = unitReadOff' (gcSpaceA s)

writeGcOff' :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeGcOff' s = unitWriteOff' (gcSpaceA s)

readComb :: ProcState -> Word -> RTL Word
readComb s =  unitRead (codeA s)

readStk :: ProcState -> Word -> RTL Word
readStk s = unitRead (stackA s)

writeStk :: ProcState -> Word -> Word -> RTL ()
writeStk s = unitWrite (stackA s)

readStkOff :: ProcState -> (OffsetReg, Int) -> RTL Word
readStkOff s = unitReadOff (stackA s)

writeStkOff :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeStkOff s = unitWriteOff (stackA s)

readStkOff' :: ProcState -> (OffsetReg, Int) -> RTL Word
readStkOff' s = unitReadOff' (stackA s)

writeStkOff' :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeStkOff' s = unitWriteOff' (stackA s)

readAStk :: ProcState -> Word -> RTL Word
readAStk s = unitRead (astackA s)

readAStkOff :: ProcState -> (OffsetReg, Int) -> RTL Word
readAStkOff s = unitReadOff (astackA s)

readAStkOff' :: ProcState -> (OffsetReg, Int) -> RTL Word
readAStkOff' s = unitReadOff' (astackA s)

writeAStk :: ProcState -> Word -> Word -> RTL ()
writeAStk s = unitWrite (astackA s)

writeAStkOff :: ProcState -> Word -> (OffsetReg, Int) -> RTL ()
writeAStkOff s = unitWriteOff (astackA s)
