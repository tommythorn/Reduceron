module MemTest.Example where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import MemTest.ProcState
import MemTest.Mem
import MemTest.Widths

a!f = f a

example :: ProcState -> RTL ()
example s =
  do (s!d1) <-- word 1

     count <- readVar (s!d1)

     while (count /</ word 30) $
       do valA <- quadRead (s!m12A) (trunc12 count)
          valB <- quadRead (s!m12B) (trunc12 count /+/ addr12 4)
          tick
          tick
          --quadWrite (s!m4A) valA (mask 0xf) (trunc4 count)
          --quadWrite (s!m4B) valB (mask 0xf) (trunc4 count /+/ addr4 4)
          quadWrite (s!m4A) valA (mask 0xf) (trunc4 count)
          quadWrite (s!m4B) valB (mask 0xf) (trunc4 count /+/ addr4 4)
          (s!d1) <-- (count /+/ word 8)
          tick

     value <- quadRead (s!m4A) (addr4 20)
     tick
     (s!d0) <-- head value
     (s!haltFlag) <-- [high]
     stop


example2 :: ProcState -> RTL ()
example2 s =
  do valA <- quadRead (s!m32A) (addr32 1)
     valB <- quadRead (s!m32B) (addr32 10)
     tick
     tick
     quadWrite (s!m12A) valA (mask 0xf) (addr12 2)
     quadWrite (s!m12B) valB (mask 0xf) (addr12 9)
     tick
     valA <- quadRead (s!m12A) (addr12 2)
     valB <- quadRead (s!m12B) (addr12 9)
     tick
     let res = tree (/+/) (zipWith (/+/) valB valA)
     (s!d0) <-- res
     (s!haltFlag) <-- [high]
     stop
