module MemTest.ProcState where

import Lava
import CircLib.Bit
import CircLib.Word
import CircLib.RTL
import CircLib.OffsetReg
import CircLib.CascadedRam
import CircLib.BlockRam
import MemTest.Mem
import MemTest.Widths

data ProcState =  PS { a4_0       :: Var     -- 4k mem address registers
                     , a4_1       :: Var

                     , a12_0      :: Var     -- 12k mem address registers
                     , a12_1      :: Var

                     , a32_0      :: Var     -- 32k mem address registers
                     , a32_1      :: Var

                     , d0         :: Var     -- Data registers
                     , d1         :: Var
                     , d2         :: Var
                     , d3         :: Var

                     , q0         :: Var     -- Quad registers
                     , q1         :: Var

                     , haltFlag   :: Var     -- Process halted

                     , m4A        :: Mem     -- Memories
                     , m4B        :: Mem
                     , m12A       :: Mem
                     , m12B       :: Mem
                     , m32A       :: Mem
                     , m32B       :: Mem
                     }

procState      :: [Int] -> RTL ProcState
procState code =
  do reg_a4_0 <- newReg addr4Width
     reg_a4_1 <- newReg addr4Width

     reg_a12_0 <- newReg addr12Width
     reg_a12_1 <- newReg addr12Width

     reg_a32_0 <- newReg addr32Width
     reg_a32_1 <- newReg addr32Width

     reg_d0 <- newReg wordSize
     reg_d1 <- newReg wordSize
     reg_d2 <- newReg wordSize
     reg_d3 <- newReg wordSize

     reg_q0 <- newReg (wordSize * 4)
     reg_q1 <- newReg (wordSize * 4)

     halt <- newReg 1

     (mem4A, mem4B) <- createMem False addr4Width (dualWide4k [])
     (mem12A, mem12B) <- createMem True addr12Width (dualWide12k [1..1000])
     (mem32A, mem32B) <- createMem True addr32Width (dualWide32k code)

     return $ PS { a4_0 = reg_a4_0
                 , a4_1 = reg_a4_1

                 , a12_0 = reg_a12_0
                 , a12_1 = reg_a12_1

                 , a32_0 = reg_a32_0
                 , a32_1 = reg_a32_1

                 , d0 = reg_d0
                 , d1 = reg_d1
                 , d2 = reg_d2
                 , d3 = reg_d3

                 , q0 = reg_q0
                 , q1 = reg_q1

                 , haltFlag = halt

                 , m4A = mem4A
                 , m4B = mem4B
                 , m12A = mem12A
                 , m12B = mem12B
                 , m32A = mem32A
                 , m32B = mem32B
                 }
