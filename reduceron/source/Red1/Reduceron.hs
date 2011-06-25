module Red1.Reduceron where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL
import CircLib.OffsetReg
import Red1.ProcState
import Red1.Mem
import Red1.Bytecode

reduceron :: ProcState -> RTL ()
reduceron s =  initialise s >> loop (step s)

initialise :: ProcState -> RTL ()
initialise s =  do reg (hp s) <-- address (hpInit s)
                   reg (sp s) <-- maxAddr
                   writeMem s (mkFunNode (word 0)) maxAddr
                   tick

step :: ProcState -> RTL ()
step s =  do stkPtr <- readVar (reg (sp s))
             top <- readMem s stkPtr

             let fun  = isFunNode top
             let func = fun <&> inv (isPrim top)
             let prim = fun <&> isPrim top

             choose
               [ isIntNode top --> handleInt s top
               , prim          --> handlePrim s top
               , func          --> handleFun s top
               , isApNode top  --> do increment (sp s) 1 ; unwind s top
               ]

handleInt :: ProcState -> Word -> RTL ()
handleInt s top = do stackPtr <- readVar (reg (sp s))

                     let fin = tree (<&>) stackPtr

                     choose
                       [ fin     --> do r0 s <-- (top `shre` 3)
                                        halted s <-- [high]
                                        stop
                       , inv fin --> do r1 s <-- top
                                        x <- readMemOff s (sp s, 1)

                                        writeMemOff s x (sp s, 0)

                                        val <- readVar (r1 s)
                                        writeMemOff s val (sp s, 1)
                       ]

handlePrim :: ProcState -> Word -> RTL ()
handlePrim s top = do r0 s <-- top
                      v1 <- readMemOff s (sp s, 1)
                      r1 s <-- (v1 `shre` 3)

                      top <- readVar (r0 s)
                      a <- readVar (r1 s)
                      v2 <- readMemOff s (sp s, 2)
                      let b = v2 `shre` 3

                      let resEq = a /=/ b
                          resNotEq = inv resEq
                          resLessEq = a /<=/ b
                          arith = isAdd top <|> isSub top
                          res = isAdd top ? (a /+/ b, a /-/ b)

                      let resTrue  =  (isEq top <&> resEq)
                                  <|> (isNotEq top <&> resNotEq)
                                  <|> (isLessEq top <&> resLessEq)

                      choose
                        [ arith --> writeMemOff' s (mkIntNode res) (sp s, 2)
                        , inv arith -->
                            choose
                              [ resTrue --> shift
                              , inv resTrue --> (increment (sp s) 4 >> tick)
                              ]
                        ]
  where
    shift = do val <- readMemOff s (sp s, 3)
               writeMemOff' s val (sp s, 4)

handleFun :: ProcState -> Word -> RTL ()
handleFun s top = unfold s top

unwind :: ProcState -> Word -> RTL ()
unwind s top = do a0 s <-- drop 3 top

                  end <- liftM head (readVar (bit0 s))
                  addr <- readVar (a0 s)
                  tick

                  doWhile (inv end) $
                    do val <- readMem s addr
                       bit0 s <-- [isEnd val]
                       a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                       writeMemOff' s val (sp s, -1)

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
unfold s top = do let initialAddr = drop 4 top ++ [low]
                  
                  a0 s <-- (initialAddr /+/ (1 `ofWidth` addrWidth))
                  addr <- readVar (a0 s)

                  heapPtr <- readVar (reg (hp s))
                  a1 s <-- heapPtr

                  start <- readMem s initialAddr
                  nib0 s <-- take 4 start
                  nib1 s <-- drop 14 start
                  dec0 s <-- take 10 (drop 4 start)
                  numArgs <- readVar (nib0 s)
                  spineLen <- readVar (nib1 s)
                  size <- readVar (dec0 s)
                  tick

                  while (tree (<|>) size) $
                    do dec0 s <-- (size /-/ (1 `ofWidth` 10))
                       a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                       node <- readMem s addr

                       trAndCopy s node


                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)
                  reg (hp s) <-- lastAddr

                  stkPtr <- readVar (reg (sp s))
                  reg (sp s) <-- (stkPtr /+!/ (numArgs ++ replicate 11 low))

                  unwind s (replicate 3 low ++ lastAddr)
