module Red8.Reduceron where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import Red8.ProcState
import Red8.Mem
import Red8.Bytecode
import Red8.Collect

reduceron :: ProcState -> RTL ()
reduceron s =  initialise s >> loop (step s)

initialise :: ProcState -> RTL ()
initialise s =  do reg (sp s) <-- maxStkAddr
                   top s <-- mkFunNode (word 0)
                   tick

step :: ProcState -> RTL ()
step s =  do t <- readVar (top s)

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
                                    increment (sp s) 1
                                    unwind s t
               ]

heapCheck :: ProcState -> RTL Bit
heapCheck s = do heapPtr <- readVar (reg (hp s))
                 return (tree (<&>) (drop 7 heapPtr))

handleInt :: ProcState -> Word -> RTL ()
handleInt s t = do stackPtr <- readVar (reg (sp s))

                   let fin = tree (<&>) stackPtr

                   choose
                     [ fin     --> do r0 s <-- (t `shre` 3)
                                      tick
                                      status s <-- statusHalted
                                      stop
                     , inv fin --> do x <- readStkOff s (sp s, 1)
                                      tick
                                      writeStkOff s t (sp s, 1)
                                      top s <-- x
                                      tick
                     ]

handlePrim :: ProcState -> Word -> RTL ()
handlePrim s t = do x <- readStkOff s (sp s, 1)
                    tick
                    r1 s <-- (x `shre` 3)
                    y <- readStkOff s (sp s, 2)
                    tick

                    a <- readVar (r1 s)
                    let b = y `shre` 3

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
                             root <- readAStkOff' s (sp s, 2)
                             tick
                             onlyIf (head root) $
                               do writeMem s (markEnd [high] result)
                                             (take addrWidth (tail root))
                                  tick
                      , inv arith -->
                          choose
                            [ resTrue -->
                                do z <- readStkOff s (sp s, 3)
                                   tick
                                   increment (sp s) 4
                                   top s <-- z
                                   tick
                            , inv resTrue -->
                                do val <- readStkOff' s (sp s, 4)
                                   tick
                                   top s <-- val
                                   tick
                            ]
                      ]

handleFun :: ProcState -> Word -> RTL ()
handleFun s t = unfold s t

unwind :: ProcState -> Word -> RTL ()
unwind s t = do let topAddr = drop 3 t
                a0 s <-- (topAddr /+/ (1 `ofWidth` addrWidth))
                a1 s <-- topAddr
                addr <- readVar (a0 s)
                root <- readVar (a1 s)
                t <- readVar (top s)
                let notEnd = inv (isEnd t)
                stkPtr <- readVar (reg (sp s))
                bit0 s <-- [high]
                val <- readMem s topAddr
                tick
                readMem s addr
                a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                tick

                first <- liftM head (readVar (bit0 s))
                let astkVal = first ? (mkAStkAddr root, word 0)

                doWhile notEnd $
                  do top s <-- val
                     writeAStkOff s astkVal (sp s, -1)
                     bit0 s <-- [low]
                     writeStkOff' s val (sp s, -1)
                     readMem s addr
                     a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                     tick

trAndCopy :: ProcState -> Word -> RTL ()
trAndCopy s node = do let end = isEnd node
                      bit0 s <-- [end]
                      end' <- readVar (bit0 s)
                      stkPtr <- readVar (reg (sp s))
                      base <- readVar (a1 s)

                      let argAddr = stkPtr /+!/ take stkAddrWidth (getArg node)
                          ap      = (base /+/ getAp node)
                                          /-/  (1 `ofWidth` addrWidth)
                          isArg   = isArgNode node
                          isAp    = isApNode node
                          isOther = isFunNode node <|> isIntNode node

                      choose
                        [ isArg -->
                            do arg <- readStk s argAddr
                               bit1 s <-- [high]
                               r1 s <-- combDatin s
                               tick
                               increment (hp s) 1
                               writeMemOff s (markEnd end' arg) (hp s, 0)
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
                  stall <- liftM head (readVar (bit1 s))
                  bit1 s <-- [low]

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
                       choose
                         [ stall --> (bit1 s <-- [low])
                         , inv stall --> (r1 s <-- node)
                         ]
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
