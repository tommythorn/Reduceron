module Red12.Collect where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import Red12.ProcState
import Red12.Mem
import Red12.Bytecode
import Red12.Compat

collect :: ProcState -> RTL ()
collect s = do gchp s <-- address 0
               sa0 s <-- maxStkAddr
               ptr <- readVar (sa0 s)
               stkPtr <- readVar (reg (sp s))
               res <- readVar (a1 s)
               t <- readVar (top s)
               status s <-- statusGc
               writeStk s t stkPtr
               tick

               while (stkPtr |<=| ptr) $
                 do node <- readStk s ptr
                    tick
                    tick
                    onlyIf (isApNode node) $
                      do copyAp s (getAp node)
                         writeStk s (mkApNode res) ptr
                    sa0 s <-- (ptr /-/ stkAddress 1)
                    tick

               fix s

               sa0 s <-- maxStkAddr
               tick

               while (stkPtr |<=| ptr) $
                 do root <- readAStk s ptr
                    tick
                    onlyIf (head root) $
                      do node <- readMem s (getAddr root)
                         tick
                         tick
                         let newRoot = isGcNode node ?
                                         (mkAStkAddr (getAp node), word 0)
                         writeAStk s newRoot ptr
                    
                    sa0 s <-- (ptr /-/ stkAddress 1)
                    tick

               newhp <- readVar (gchp s)
               reg (hp s) <-- newhp

               -- Pipelined copy back
               gchp s <-- (newhp /-/ address 1)
               tick
               node <- readGc s newhp
               tick

               while (inv (tree (<&>) newhp)) $
                 do let newhp' = (newhp /-/ address 1)
                    readGc s newhp'
                    gchp s <-- newhp'
                    tick
                    writeMem s node (newhp /+/ address 1)

               newTop <- readStk s stkPtr -- CAN BE AVOIDED?
               tick
               tick
               top s <-- newTop
               tick
               quadRead (stackA s) (stkPtr /+/ stkAddress 1)
               quadRead (stackB s) (stkPtr /+/ stkAddress 5)
               --zipWithM_ (<--) (top8 s) (valA ++ valB)
               octoRead (heapA s, heapB s) (drop 3 t)
               tick

-- The "fix" routine should be pipelined to double efficiency!

fix :: ProcState -> RTL ()
fix s =  do a2 s <-- address 0
            ptr <- readVar (a2 s)
            res <- readVar (a1 s)
            gchpPtr <- readVar (gchp s)
            end <- readVar (bit0 s)
            tick

            while (ptr |<| gchpPtr) $
              do node <- readGc s ptr
                 tick
                 tick
                 bit0 s <-- [isEnd node]
                 onlyIf (isApNode node) $
                   do copyAp s (getAp node)
                      writeGc s (markEnd end (mkApNode res)) ptr
                 a2 s <-- (ptr /+/ address 1)
                 tick
          

copyAp :: ProcState -> Word -> RTL ()
copyAp s src = do gchpPtr <- readVar (gchp s)
                  a0 s <-- src
                  a1 s <-- gchpPtr
                  a3 s <-- src
                  origSrc <- readVar (a0 s)
                  addr <- readVar (a1 s)
                  ptr <- readVar (a3 s)
                  node <- readMem s src
                  end <- readVar (bit1 s)
                  tick
                  tick

                  let collected = isGcNode node

                  choose
                    [ collected -->
                        do a1 s <-- getAp node
                           tick
                    , inv collected -->
                        do doWhile (inv (isEnd node)) $
                             do readMem s ptr
                                tick
                                a3 s <-- (ptr /+/ address 1)
                                tick
                                gchp s <-- (gchpPtr /+/ address 1)
                                writeGc s node gchpPtr
                           writeMem s (mkGcNode addr) origSrc
                           tick
                    ]
