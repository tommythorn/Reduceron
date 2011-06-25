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
reduceron s =
  do uw <- initialise s
     loop (step s uw)

initialise :: ProcState -> RTL Label
initialise s =  do reg (hp s) <-- address (hpInit s)
                   reg (sp s) <-- maxAddr
                   top s <-- mkFunNode (word 0)
                   --uw <- share (unwind s)
                   uw <- share (return ())
                   tick
                   return uw

step s uw =  do t <- readVar (top s)

                let fun  = isFunNode t
                let func = fun <&> inv (isPrim t)
                let prim = fun <&> isPrim t

                b <- heapCheck s
                onlyIf b (collect s)

                choose
                  [ isIntNode t --> handleInt s t
                  , prim        --> handlePrim s t
                  , func        --> unfold s uw t
                  , isApNode t  --> unwind s -- call uw
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
                    root <- readMem s (asp s /+/ address 2)
                    r1 s <-- (x `shre` 3)
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
                        resLogic =  resTrue ? (trueAddr, falseAddr)

                    r2 s <-- (arith ? (mkIntNode res, resLogic))
                    c <- readVar (r2 s)

                    tick
                    top s <-- c
                    increment (sp s) 2

                    onlyIf (head root) $
                      writeMem s (markEnd [high] c)
                                 (take addrWidth (tail root))

                    tick

unwind s   = do --let topAddr = drop 3 t
                topAddr <- liftM (drop 3) (readVar (top s))
                addr <- readVar (a0 s)
                [endVal] <- readVar (bit1 s)
                [first] <- readVar (bit0 s)
                stkPtr <- readVar (reg (sp s))
                x <- readVar (r0 s)
                val <- readMem s topAddr
                a0 s <-- topAddr
                bit0 s <-- [high]
                tick

                doWhile (inv endVal) $
                  do readMem s (addr /+/ address 1)
                     tick

                     onlyIf first (r0 s <-- val)
                     bit0 s <-- [low]
                     --writeMem s (mkAStkAddr addr) (asp s /-/ address 1)
                     writeMem s (mkAStkAddr addr) (asp s)
                     tick

                     r0 s <-- val
                     top s <-- unEnd x
                     writeMem s (unEnd x) stkPtr
                     bit1 s <-- [isEnd x]
                     a0 s <-- (addr /+/ address 1)
                     onlyIf (inv (isEnd x)) (increment (sp s) (-1))
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


unfold s uw t =
               do let initialAddr = drop 4 t ++ [low]
                  stkPtr <- readVar (reg (sp s))

                  a0 s <-- (initialAddr /+/ (1 `ofWidth` addrWidth))
                  addr <- readVar (a0 s)
                  val <- readVar (r1 s)
                  [first] <- readVar (bit2 s)
                  stall <- liftM head (readVar (bit1 s))
                  bit2 s <-- [high]
                  bit1 s <-- [low]

                  heapPtr <- readVar (reg (hp s))
                  a1 s <-- heapPtr

                  start <- readMem s initialAddr
                  tick

                  node <- readMem s addr
                  a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                  tick

                  (numArgs0, numArgs, spineLen, size) <- extract start
                  let stkPtr' = stkPtr /+/ (numArgs0 ++ replicate 6 low)
                  root <- readMem s (stkPtr' /-/ address (2^10))
                  tick

                  while (tree (<|>) size) $
                    do readMem s addr
                       a0 s <-- (addr /+/ (1 `ofWidth` addrWidth))
                       choose
                         [ stall --> (bit1 s <-- [low])
                         , inv stall --> (r1 s <-- node)
                         ]
                       tick
                       bit2 s <-- [low]
                       onlyIf first (r0 s <-- root)
                       dec0 s <-- (size /-/ (1 `ofWidth` 10))
                       trAndCopy s val

                  let stkPtr' = stkPtr /+/ (numArgs ++ replicate 6 low)
                  let lastAddr = heapPtr /-/ (spineLen ++ replicate 11 low)
                  reg (sp s) <-- stkPtr'
                  top s <-- mkApNode lastAddr

                  r <- readVar (r0 s)
                  onlyIf (head r) $
                     do writeMem s (mkApEndNode lastAddr)
                                   (take addrWidth (tail r))

                  tick

                  --call uw
                  unwind s
  where
    extract start = do nib0 s <-- take 4 start
                       nib1 s <-- drop 14 start
                       dec0 s <-- take 10 (drop 4 start)
                       numArgs <- readVar (nib0 s)
                       spineLen <- readVar (nib1 s)
                       size <- readVar (dec0 s)
                       return (take 4 start, numArgs, spineLen, size)
