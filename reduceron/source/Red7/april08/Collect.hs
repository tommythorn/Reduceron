module Red7.Collect where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import Red7.ProcState
import Red7.Mem
import Red7.Bytecode
import Debug.Trace

collect :: ProcState -> RTL ()
collect s = do gchp s <-- address (hpInit s)
               sa0 s <-- maxAddr
               ptr <- readVar (sa0 s)
               stkPtr <- readVar (reg (sp s))
               res <- readVar (a1 s)
               t <- readVar (top s)
               writeMem s t stkPtr
               tick

               while (stkPtr |<=| ptr) $
                 do node <- readMem s ptr
                    tick
                    tick
                    onlyIf (isApNode node) $
                      do copyAp s (getAp node)
                         writeMem s (mkApNode res) ptr
                    sa0 s <-- (ptr /-/ address 1)
                    tick

               fix s

               sa0 s <-- astkBase
               tick

               while (asp s |<=| ptr) $
                 do root <- readMem s ptr
                    tick
                    tick
                    onlyIf (head root) $
                      do node <- readMem s (getAddr root)
                         tick
                         tick
                         let newRoot = isGcNode node ?
                                         (mkAStkAddr (getAp node), word 0)
                         writeMem s newRoot ptr
                    
                    sa0 s <-- (ptr /-/ address 1)
                    tick

               newhp <- readVar (gchp s)
               reg (hp s) <-- newhp

               -- Pipelined copy back
               gchp s <-- (newhp /-/ address 1)
               tick
               node <- readGc s newhp
               tick

               while (inv (newhp /=/ address (hpInit s - 1))) $
                 do let newhp' = (newhp /-/ address 1)
                    readGc s newhp'
                    gchp s <-- newhp'
                    tick
                    writeMem s node (newhp /+/ address 1)

               tick
               newTop <- readMem s stkPtr
               tick
               tick
               top s <-- newTop
               tick

-- The "fix" routine should be pipelined to double efficiency!

fix :: ProcState -> RTL ()
fix s =  do a2 s <-- address (hpInit s)
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
