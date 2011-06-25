module Red11.Reduceron where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import Red11.ProcState
import Red11.Mem
import Red11.Bytecode
import Red11.Collect
import Red11.Compat

reduceron :: ProcState -> RTL ()
reduceron s =
  do uw <- share (unwind s)
     initialise s
     loop (step s uw)

initialise :: ProcState -> RTL ()
initialise s =  do reg (sp s) <-- maxStkAddr
                   top s <-- mkFunNode (word 0)
                   tick
                   share (unwind s)

checkTop8 :: ProcState -> RTL ()
checkTop8 s =
  do [stale] <- readVar (topIsStale s)
     onlyIf stale $
       do let valA = memDatIn (stackA s)
          let valB = memDatIn (stackB s)
          let tops = groupN wordSize (valA ++ valB)
          zipWithM_ (<--) (top8 s) tops
          tick

incrSP :: ProcState -> Int -> RTL ()
incrSP s n =
  do stkPtr <- readVar (reg (sp s))
     topIsStale s <-- [high]
     let addrA = stkPtr /+/ stkAddress (n+1)
         addrB = stkPtr /+/ stkAddress (n+5)
     increment (sp s) n
     quadRead (stackA s) addrA
     quadRead (stackB s) addrB
     return ()

updateSP :: ProcState -> Word -> RTL ()
updateSP s stkPtr =
  do topIsStale s <-- [high]
     let addrA = stkPtr /+/ stkAddress 1
         addrB = stkPtr /+/ stkAddress 5
     reg (sp s) <-- stkPtr
     quadRead (stackA s) addrA
     quadRead (stackB s) addrB
     return ()

leaveSP :: ProcState -> RTL ()
leaveSP s = topIsStale s <-- [low]

step :: ProcState -> RTL ()
step s =  do checkTop8 s
             t <- readVar (top s)

             let fun  = isFunNode t
             let func = fun <&> inv (isPrim t)
             let prim = fun <&> isPrim t

             b <- heapCheck s
             onlyIf b (collect s)

             choose
               [ isIntNode t --> do status s <-- statusInt
                                    handleInt s t
               , prim        --> do status s <-- statusPrim
                                    handlePrim s t
               , func        --> do status s <-- statusUnfold
                                    handleFun s t
               , isApNode t  --> do status s <-- statusUnwind
                                    unwind s t
               ]

heapCheck :: ProcState -> RTL Bit
heapCheck s = do heapPtr <- readVar (reg (hp s))
                 return (tree (<&>) (drop 7 heapPtr))

handleInt :: ProcState -> Word -> RTL ()
handleInt s t =
  do stackPtr <- readVar (reg (sp s))
     t1 <- readVar (top8 s !! 0)

     let fin = tree (<&>) stackPtr

     choose
       [ fin     --> do r0 s <-- (t `shre` 3)
                        tick
                        status s <-- statusHalted
                        stop

       , inv fin --> do (top s) <-- t1
                        (top8 s !! 0) <-- t
                        writeStkOff s t (sp s, 1) -- Needed in future?
                        leaveSP s
                        tick
       ]

handlePrim :: ProcState -> Word -> RTL ()
handlePrim s t =
  do tops <- mapM readVar (top8 s)

     let a = (tops !! 0) `shre` 3
     let b = (tops !! 1) `shre` 3

     let resEq = a /=/ b
         resNotEq = inv resEq
         resLessEq = a /<=/ b
         arith = isAdd t <|> isSub t
         res = isAdd t ? (a /+/ b, a /-/ b)

     let resTrue  =  (isEq t <&> resEq)
                 <|> (isNotEq t <&> resNotEq)
                 <|> (isLessEq t <&> resLessEq)

     choose
       [ arith -->
           do top s <-- mkIntNode res          -- DO SWAP TOO
              result <- readVar (top s)
              root <- readAStkOff s (sp s, 2)  -- NOT GOOD!
              incrSP s 2
              tick
              onlyIf (head root) $
                writeMem s (markEnd [high] result)
                           (take addrWidth (tail root))

       , inv arith -->
           choose
             [ resTrue -->
                 do top s <-- (tops !! 2)
                    incrSP s 4
                    tick
                    
             , inv resTrue -->
                 do top s <-- (tops !! 3)
                    incrSP s 4
                    tick
             ]
       ]

handleFun :: ProcState -> Word -> RTL ()
handleFun s t = unfold s t

unwind :: ProcState -> Word -> RTL ()
unwind s taddr =
  do let topAddr = drop 3 taddr
     --a1 s <-- topAddr
     top s <-- taddr
     --root <- readVar (a1 s)
     stkPtr <- readVar (reg (sp s))
     t <- readVar (top s)

     nodes <- octoRead (heapA s, heapB s) topAddr
     tick
     
     while (isApNode t) $  -- CAREFUL: where's t coming from?
       do let spine = reverse nodes
          let endMarked = onlyFirst (map isEnd nodes)
          let len = encode endMarked
          let len' = len ++ replicate (stkAddrWidth - 3) low
          let stkAddrA = stkPtr /-/ stkAddress 7
          let newTop = select endMarked nodes
          let root = drop 3 t
          --let stkAddrB = stkPtr /-/ stkAddress 3

          tick
          octoWrite (stackA s, stackB s) spine stkAddrA
          octoWrite (astackA s, astackB s)
                    (replicate 7 (word 0) ++ [mkAStkAddr root])
                    stkAddrA
          octoRead (heapA s, heapB s) (drop 3 newTop)
          top s <-- newTop
          reg (sp s) <-- (stkPtr /-/ len')
          tick

     incrSP s 0
     tick

{-
unwind :: ProcState -> Word -> RTL ()  -- TODO: PIPELINE, SHARE(?)
unwind s t =
  do let topAddr = drop 3 t
     a1 s <-- topAddr
     root <- readVar (a1 s)
     stkPtr <- readVar (reg (sp s))
 
     valA <- quadRead (heapA s) topAddr
     valB <- quadRead (heapB s) (topAddr /+/ address 4)
     tick
     tick
     let spine = reverse (valA ++ valB)
     let endMarked = onlyFirst (map isEnd (valA ++ valB))
     let len = encode endMarked
     let len' = len ++ replicate (stkAddrWidth - 3) low

     let stkAddrA = stkPtr /-/ stkAddress 7
     let stkAddrB = stkPtr /-/ stkAddress 3
     quadWrite (stackA s) (take 4 spine) (mask 0xf) stkAddrA
     quadWrite (stackB s) (drop 4 spine) (mask 0xf) stkAddrB

     quadWrite (astackA s) (replicate 4 (word 0)) (mask 0xf) stkAddrA
     quadWrite (astackB s) (replicate 3 (word 0) ++ [mkAStkAddr root])
                                                       (mask 0xf) stkAddrB

     top s <-- select endMarked (valA ++ valB) -- YUCK!
     reg (sp s) <-- (stkPtr /-/ len')

     tick
     incrSP s 0
     tick
-}


inst :: ProcState -> Word -> RTL Word
inst s node = 
  do let end = [isEnd node]
     base <- readVar (a1 s)
     tops <- mapM readVar (top8 s)

     let argNum  = take 3 (getArg node)
         arg     = select (decode argNum) tops
         ap      = base /+/ (getAp node /-/ (1 `ofWidth` addrWidth))
         isArg   = isArgNode node
         isAp    = isApNode node
         isOther = isFunNode node <|> isIntNode node

         trArg   = markEnd end arg
         trAp    = markEnd end (mkApNode ap)

         output  = select ([isArg] ++ [isAp] ++ [isOther]) [trArg, trAp, node]

     return output
     
unfold :: ProcState -> Word -> RTL ()
unfold s top = do let initialAddr = take combAddrWidth (drop 4 top)
                  heapPtr <- readVar (reg (hp s))
                  a1 s <-- heapPtr
                  ca0 s <-- (initialAddr /+/ combAddress 8)
                  addr <- readVar (ca0 s)
                  size <- readVar (dec0 s)
                  stkPtr <- readVar (reg (sp s))
                  heapPtr' <- readVar (a0 s)

                  code <- octoRead (codeA s, codeB s) initialAddr
                  code' <- mapM (inst s) code
                  tick
                  octoRead (codeA s, codeB s) addr
                  ca0 s <-- (addr /+/ combAddress 8)
                  tick

                  (numArgs, spineLen, sz) <- extract (code !! 0)

                  let stkPtr' = stkPtr /+/ (numArgs ++ replicate 6 low)
                  reg (sp s) <-- stkPtr'
                  root <- readAStk s stkPtr'   -- SLOW?

                  octoWrite (heapA s, heapB s) (rotl 1 code') heapPtr
                  dec0 s <-- (sz /-/ (7 `ofWidth` 10))
                  let hpInitIncr = wordMin sz (7 `ofWidth` 10)
                  reg (hp s) <-- (heapPtr /+/ hpInitIncr)

                  octoRead (codeA s, codeB s) addr
                  ca0 s <-- (addr /+/ combAddress 8)
                  tick

                  let hpIncr = wordMin size (8 `ofWidth` 10)
                  
                  while (address 0 /</ size) $
                    do octoWrite (heapA s, heapB s) code' heapPtr
                       octoRead (codeA s, codeB s) addr
                       reg (hp s) <-- (heapPtr /+/ hpIncr)
                       ca0 s <-- (addr /+/ combAddress 8)
                       dec0 s <-- (size /-/ (8 `ofWidth` 10))
                       readAStk s stkPtr
                       tick

                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)

                  onlyIf (head root) $
                     do writeMem s (mkApEndNode lastAddr)
                                   (take addrWidth (tail root))
                        tick


                  --unwind s (replicate 3 low ++ lastAddr)
                  unwind s (mkApNode lastAddr)
  where
    extract start = do let numArgs = take 4 start
                       nib1 s <-- drop 14 start
                       let size = take 10 (drop 4 start)
                       spineLen <- readVar (nib1 s)
                       return (numArgs, spineLen, size)
