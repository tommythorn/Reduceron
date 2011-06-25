module Red10.Reduceron where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import Red10.ProcState
import Red10.Mem
import Red10.Bytecode
import Red10.Collect
import Red10.Compat

reduceron :: ProcState -> RTL ()
reduceron s =  initialise s >> loop (step s)

initialise :: ProcState -> RTL ()
initialise s =  do reg (sp s) <-- maxStkAddr
                   top s <-- mkFunNode (word 0)
                   tick

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
                                    increment (sp s) 1 -- WATCH OUT!
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
           do top s <-- mkIntNode res
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

     let stkAddrA = stkPtr /-/ stkAddress 8
     let stkAddrB = stkPtr /-/ stkAddress 4
     quadWrite (stackA s) (take 4 spine) (mask 0xf) stkAddrA
     quadWrite (stackB s) (drop 4 spine) (mask 0xf) stkAddrB

     quadWrite (astackA s) (replicate 4 (word 0)) (mask 0xf) stkAddrA
     quadWrite (astackB s) (replicate 3 (word 0) ++ [mkAStkAddr root])
                                                       (mask 0xf) stkAddrB

     top s <-- select endMarked (valA ++ valB) -- YUCK!
     reg (sp s) <-- (stkPtr /-/ (len' /+/ stkAddress 1))

     tick
     incrSP s 0
     tick

--   Alternative for last 3 lines:
--   reg (sp s) <-- (stkPtr /-/ len')
--   tick
--   decrSP s 2


{-
trAndCopy :: ProcState -> Word -> RTL ()
trAndCopy s node = do let end = isEnd node
                      stkPtr <- readVar (reg (sp s))
                      base <- readVar (a1 s)

                      let argAddr = stkPtr /+!/ take stkAddrWidth (getArg node)
                          ap      = (base /+/ getAp node)
                                          /-/  (1 `ofWidth` addrWidth)
                          isArg   = isArgNode node
                          isAp    = isApNode node
                          isOther = isFunNode node <|> isIntNode node

                      tops <- mapM readVar (top8 s)

                      let argNum = take 3 (getArg node)
                          arg = select (decode argNum) tops

                      choose
                        [ isArg -->
                            do increment (hp s) 1
                               writeMemOff s (markEnd [end] arg) (hp s, 0)
                        , isAp -->
                            do increment (hp s) 1
                               let apNode = mkApNode ap
                               writeMemOff s (markEnd [end] apNode) (hp s, 0)
                        , isOther -->
                            do increment (hp s) 1
                               writeMemOff s node (hp s, 0)
                        ]

unfold :: ProcState -> Word -> RTL ()
unfold s top = do let initialAddr = take combAddrWidth (drop 4 top)
                  
                  ca0 s <-- (initialAddr /+/ (1 `ofWidth` combAddrWidth))
                  addr <- readVar (ca0 s)
                  val <- readVar (r1 s)

                  heapPtr <- readVar (reg (hp s))
                  a1 s <-- heapPtr

                  start <- readComb s initialAddr
                  tick

                  (numArgs, spineLen, size) <- extract start
                  node <- readComb s addr
                  ca0 s <-- (addr /+/ (1 `ofWidth` combAddrWidth))
                  tick

                  while (tree (<|>) size) $
                    do ca0 s <-- (addr /+/ (1 `ofWidth` combAddrWidth))
                       dec0 s <-- (size /-/ (1 `ofWidth` 10))
                       readComb s addr
                       r1 s <-- node
                       tick

                       trAndCopy s val

                  stkPtr <- readVar (reg (sp s))
                  let stkPtr' = stkPtr /+/ (numArgs ++ replicate 6 low)
                  reg (sp s) <-- (stkPtr' /+/ (1 `ofWidth` 10))
                  root <- readAStk s stkPtr'
                  tick

                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)

                  onlyIf (head root) $
                     do writeMem s (mkApEndNode lastAddr)
                                   (take addrWidth (tail root))
                        tick


                  unwind s (replicate 3 low ++ lastAddr)
  where
    extract start = do nib0 s <-- take 4 start
                       nib1 s <-- drop 14 start
                       dec0 s <-- take 10 (drop 4 start)
                       numArgs <- readVar (nib0 s)
                       spineLen <- readVar (nib1 s)
                       size <- readVar (dec0 s)
                       return (numArgs, spineLen, size)
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
                  heapPtr' <- readVar (a0 s)

                  code <- octoRead (codeA s, codeB s) initialAddr
                  code' <- mapM (inst s) code
                  tick
                  tick

                  (numArgs, spineLen, sz) <- extract (code !! 0)
                  octoWrite (heapA s, heapB s) (rotl 1 code') heapPtr
                  dec0 s <-- (sz /-/ (7 `ofWidth` 10))
                  let hpInitIncr = wordMin sz (7 `ofWidth` 10)
                  reg (hp s) <-- (heapPtr /+/ hpInitIncr)
                  tick

                  let hpIncr = wordMin size (8 `ofWidth` 10)
                  
                  while (address 0 /</ size) $
                    do dec0 s <-- (size /-/ (8 `ofWidth` 10))
                       reg (hp s) <-- (heapPtr /+/ hpIncr)
                       a0 s <-- heapPtr
                       ca0 s <-- (addr /+/ combAddress 8)
                       octoRead (codeA s, codeB s) addr
                       tick  -- PIPELINE ME!
                       tick
                       octoWrite (heapA s, heapB s) code' heapPtr'

                  stkPtr <- readVar (reg (sp s))
                  let stkPtr' = stkPtr /+/ (numArgs ++ replicate 6 low)
                  reg (sp s) <-- (stkPtr' /+/ (1 `ofWidth` 10))
                  root <- readAStk s stkPtr'
                  tick

                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)

                  onlyIf (head root) $
                     do writeMem s (mkApEndNode lastAddr)
                                   (take addrWidth (tail root))
                        tick


                  unwind s (replicate 3 low ++ lastAddr)
  where
    extract start = do nib0 s <-- take 4 start
                       nib1 s <-- drop 14 start
                       let size = take 10 (drop 4 start)
                       numArgs <- readVar (nib0 s)
                       spineLen <- readVar (nib1 s)
                       return (numArgs, spineLen, size)
