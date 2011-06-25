module Red7.Reduceron where

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
import Red7.Collect

reduceron :: ProcState -> RTL ()
reduceron s =  initialise s >> loop (step s)

initialise :: ProcState -> RTL ()
initialise s =  do reg (hp s) <-- address (hpInit s)
                   reg (sp s) <-- maxAddr
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
               [ isIntNode t --> handleInt s t
               , prim        --> handlePrim s t
               , func        --> handleFun s t
               , isApNode t  --> do increment (sp s) 1 ; unwind s t
               ]

heapCheck :: ProcState -> RTL Bit
heapCheck s = do heapPtr <- readVar (reg (hp s))
                 return (tree (<&>) (drop 11 heapPtr))

handleInt :: ProcState -> Word -> RTL ()
handleInt s t = do stackPtr <- readVar (reg (sp s))

                   let fin = tree (<&>) stackPtr

                   choose
                     [ fin     --> do r0 s <-- (t `shre` 3)
                                      halted s <-- [high]
                                      stop
                     , inv fin --> do x <- readMemOff s (sp s, 1)
                                      tick
                                      writeMemOff s t (sp s, 1)
                                      tick
                                      top s <-- x
                                      tick
                     ]

handlePrim :: ProcState -> Word -> RTL ()
handlePrim s t = do x <- readMemOff s (sp s, 1)
                    tick
                    y <- readMemOff s (sp s, 2)
                    tick
                    r1 s <-- (x `shre` 3)
                    z <- readMemOff s (sp s, 3)
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

                    root <- readMem s (asp s /+/ address 2)

                    choose
                      [ arith -->
                          do result <- readVar (top s)
                             increment (sp s) 2
                             top s <-- mkIntNode res
                             tick
                             tick
                             onlyIf (head root) $
                               do writeMem s (markEnd [high] result)
                                             (take addrWidth (tail root))
                                  tick
                      , inv arith -->
                          choose
                            [ resTrue -->
                                do tick
                                   increment (sp s) 4
                                   top s <-- z
                                   tick
                                   onlyIf (head root) $
                                     do writeMem s (markEnd [high] trueAddr)
                                                   (take addrWidth (tail root))
                                        tick

                            , inv resTrue -->
                                do tick
                                   val <- readMemOff' s (sp s, 4)
                                   tick
                                   onlyIf (head root) $
                                     do writeMem s (markEnd [high] falseAddr)
                                                   (take addrWidth (tail root))
                                   tick
                                   top s <-- val
                                   tick
                            ]
                      ]


{-
handlePrim :: ProcState -> Word -> RTL ()
handlePrim s t = do x <- readMemOff s (sp s, 1)
                    tick
                    y <- readMemOff s (sp s, 2)
                    tick
                    r1 s <-- (x `shre` 3)
                    z <- readMemOff s (sp s, 3)
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
                          do result <- readVar (top s)
                             root <- readMem s (asp s /+/ address 2)
                             increment (sp s) 2
                             top s <-- mkIntNode res
                             tick
                             tick
                             onlyIf (head root) $
                               do writeMem s (markEnd [high] result)
                                             (take addrWidth (tail root))
                                  tick
                      , inv arith -->
                          choose
                            [ resTrue -->
                                do tick
                                   increment (sp s) 4
                                   top s <-- z
                                   tick
                            , inv resTrue -->
                                do val <- readMemOff' s (sp s, 4)
                                   tick
                                   tick
                                   top s <-- val
                                   tick
                            ]
                      ]
-}

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
                tick

                first <- liftM head (readVar (bit0 s))
                let astkVal = mkAStkAddr root

                doWhile notEnd $
                  do writeMem s astkVal (asp s /-/ address 1)
                     a1 s <-- (root /+/ (1 `ofWidth` addrWidth))
                     bit0 s <-- [low]
                     top s <-- val
                     tick
                     readMem s addr
                     a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                     increment (sp s) (-1)
                     tick
                     onlyIf notEnd $
                       do writeMem s t stkPtr
                          tick

{-
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
                tick

                first <- liftM head (readVar (bit0 s))
                let astkVal = first ? (mkAStkAddr root, word 0)

                doWhile notEnd $
                  do writeMem s astkVal (asp s /-/ address 1)
                     bit0 s <-- [low]
                     top s <-- val
                     tick
                     readMem s addr
                     a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                     increment (sp s) (-1)
                     tick
                     onlyIf notEnd $
                       do writeMem s t stkPtr
                          tick
-}

trAndCopy :: ProcState -> Word -> RTL ()
trAndCopy s node = do let end = isEnd node
                      bit0 s <-- [end]
                      end' <- readVar (bit0 s)
                      stkPtr <- readVar (reg (sp s))
                      base <- readVar (a1 s)

                      let argAddr = stkPtr /+!/ getArg node
                          ap      = (base /+/ getAp node)
                                          /-/  (1 `ofWidth` addrWidth)
                          isArg   = isArgNode node
                          isAp    = isApNode node
                          isOther = isFunNode node <|> isIntNode node


                      choose
                        [ isArg -->
                            do arg <- readMem s argAddr
                               tick
                               bit1 s <-- [high]
                               r1 s <-- arg
                               tick
                               increment (hp s) 1
                               writeMemOff s (markEnd end' arg) (hp s, 0)
                               tick
                        , isAp -->
                            do increment (hp s) 1
                               let apNode = mkApNode ap
                               writeMemOff s (markEnd [end] apNode) (hp s, 0)
                               tick
                        , isOther -->
                            do increment (hp s) 1
                               writeMemOff s node (hp s, 0)
                               tick
                        ]



unfold :: ProcState -> Word -> RTL ()
unfold s top = do let initialAddr = drop 4 top ++ [low]
                  
                  a0 s <-- (initialAddr /+/ (1 `ofWidth` addrWidth))
                  addr <- readVar (a0 s)
                  val <- readVar (r1 s)
                  stall <- liftM head (readVar (bit1 s))
                  bit1 s <-- [low]

                  heapPtr <- readVar (reg (hp s))
                  a1 s <-- heapPtr

                  start <- readMem s initialAddr
                  tick

                  node <- readMem s addr
                  a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                  tick

                  (numArgs, spineLen, size) <- extract start
                  tick

                  while (tree (<|>) size) $
                    do readMem s addr
                       a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                       choose
                         [ stall --> (bit1 s <-- [low])
                         , inv stall --> (r1 s <-- node)
                         ]
                       tick
                       dec0 s <-- (size /-/ (1 `ofWidth` 10))
                       trAndCopy s val

                  stkPtr <- readVar (reg (sp s))
                  let stkPtr' = stkPtr /+/ (numArgs ++ replicate 6 low)
                  reg (sp s) <-- (stkPtr' /+/ (1 `ofWidth` 10))
                  root <- readMem s (stkPtr' /-/ address (2^10))
                  tick
                  tick

                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)

                  onlyIf (head root) $
                     do writeMem s (mkApEndNode lastAddr)
                                   (take addrWidth (tail root))
                        tick

                  unwind s (replicate 3 low ++ lastAddr)

{-
                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)
                  reg (hp s) <-- lastAddr

                  stkPtr <- readVar (reg (sp s))
                  reg (sp s) <-- (stkPtr /+!/ (numArgs ++ replicate 11 low))

                  unwind s (replicate 3 low ++ lastAddr)
-}
  where
    extract start = do nib0 s <-- take 4 start
                       nib1 s <-- drop 14 start
                       dec0 s <-- take 10 (drop 4 start)
                       numArgs <- readVar (nib0 s)
                       spineLen <- readVar (nib1 s)
                       size <- readVar (dec0 s)
                       return (numArgs, spineLen, size)
