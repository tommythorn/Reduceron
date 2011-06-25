module Red16.Reduceron where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL2
import Red16.Widths
import Red16.ProcState
import Red16.Mem
import Red16.Bytecode
import Red16.Collect

-- The Reduceron is defined to be repeated application of "step".

reduceron s = initialise s >> loop (step s)

-- By default, all flip-flops initialise to low.  The top of
-- stack needs to point to the main function, and the stack pointer
-- needs to be initialised to all ones, as the stack grows downwards.

initialise s =
  do s!top <== mkFunNode (word 0) -- main is at address 0
     tick

-- The "step" function decides how to proceed based on the type of node
-- sitting on top of the stack.  If the stack contains a single Int node
-- then the Reduceron terminates.  When the heap is all but 128 nodes
-- full, the garbage collector is invoked.

step r =
  do onlyIf heapFull (r!collect)
     choose
       [ t!isInt  --> ifte stackEmpty (do r!status <== statusHalted ; stop)
                                      (r!swap)
       , t!isPrim --> r!prim
       , t!isFunc --> r!unfold
       , t!isAp   --> r!unwind
       ]
  where
    t          = r!top!val
    heapFull   = tree (<&>) (drop 7 (r!hp!val))
    stackEmpty = inv $ tree (<|>) (r!sp!val)

-- The top two elements of the stack are swapped by "swap".  Notice
-- that octoWrite has the effect of both writing to memory and reading
-- from it at the same time, since block RAMs contain two seperate
-- data busses (one for writing and one for reading).

swap r =
  do tick

     r!top <== newTop

     unitWrite (r!stack!fst) (r!top!val) (r!sp!val)
     quadRead (r!stack!snd) (r!spPlus4)
     octoRead (r!heap) (newTop!apAddr)
     octoRead (r!code) (newTop!funAddr)

     tick
  where
    newTop = (r!stack!octoOut) !! 0

{-
-- The ALU (arithmetic & logic unit) is purely combinatorial logic.
-- It examines the binary operator and its arguments (at the top 3
-- positions on the stack) and computes the result.

alu r = t!isLogOp ? (mkBoolNode logRes, mkIntNode arithRes)
  where
    t  = r!top!val -- top of stack
    ts = r!stack!octoOut -- next top 8 of stack

    a  = (ts !! 0)!intVal -- first operand
    b  = (ts !! 1)!intVal -- second operand

    eq  = a /=/ b
    leq = a /<=/ b

    -- logical result:
    logRes  =  (t!isEqOp <&> eq) 
           <|> (t!isNeqOp <&> inv eq)
           <|> (t!isLeqOp <&> leq)

    -- arithmetic result:
    arithRes = (t!isAddOp) ? (a /+/ b, a /-/ b)

-- The "prim" function handles primitive applications by writing the
-- output of the ALU to the top of the stack, remembering to increment
-- the stack pointer and update the root of the redex on the heap.

prim s =
  do tick

     s!top <== s!alu
     s!sp <== s!sp!val /+/ unsigned 2

     tick

     quadRead (s!stack!fst) (s!sp!val)
     quadRead (s!stack!snd) (s!spPlus4)
     octoRead (s!heap) (s!top!val!apAddr)
     octoRead (s!code) (s!top!val!funAddr)
     unitRead (s!astack!fst) (s!sp!val /-/ unsigned 1)

     tick

     let root = s!astack!unitOut
     onlyIf (head root) $
       unitWrite (s!heap!fst) (markEnd [high] (s!top!val))
                              (rootAddr root)
-}


-- Optimised variants

-- This code optimises primitive handling in two ways:
--   * does combined evaluate/swap on arithmetic primitives
--   * does straight jump to true/false alternative on logical primitives

alu r =
  do r!top <== newTop
     r!sp <== r!sp!val /+/ spIncr
     r!aluRes <== cmp ? (mkBoolNode resCmp, mkIntNode resArith)
     r!aluArith <== [inv cmp]
  where
    t  = r!top!val
    ts = r!stack!octoOut

    a  = (ts !! 0)!intVal
    b  = (ts !! 1)!intVal

    eq  = a /=/ b
    leq = a /<=/ b

    cmp = t!isEqOp <|> t!isNeqOp <|> t!isLeqOp

    resCmp  =  (t!isEqOp <&> eq) 
           <|> (t!isNeqOp <&> inv eq)
           <|> (t!isLeqOp <&> leq)

    resArith = (t!isAddOp) ? (a /+/ b, a /-/ b)

    newTop = (cmp <&> inv resCmp) ? (ts !! 3, ts !! 2)

    spIncr = cmp ? (4 `ofWidth` 4, 2 `ofWidth` 4)

prim s =
  do tick
     alu s
     quadRead (s!astack!fst) (s!sp!val /+/ unsigned 1)
     tick

     quadWrite (s!stack!fst)
               (s!aluRes!val:replicate 3 (replicate wordSize low))
               [s!aluArith!val!head,low,low,low]
               (s!sp!val)
     quadRead (s!stack!snd) (s!sp!val /+/ unsigned 4)
     octoRead (s!heap) (t!apAddr)
     octoRead (s!code) (t!funAddr)

     onlyIf (s!aluArith!val!head <&> stackEmpty) (top s <== s!aluRes!val)

     s!root <== s!astack!unitOut

     tick

     onlyIf (s!root!val!head) $
       unitWrite (s!heap!fst) (markEnd [high] (s!aluRes!val))
                 (s!root!val!rootAddr)
--       writeMem s (markEnd [high] (s!aluRes!val))
--                  (s!root!val!rootAddr)
  where
    t = s!top!val
    stackEmpty = inv $ tree (<|>) (s!sp!val)


-- Unwind copies the sequence of nodes pointed to by the application
-- node on to the top of the stack.

unwind s =
  do s!sp2 <== s!sp!val /-/ unsigned 8

     tick

     s!top <== newTop
     s!sp <== s!sp!val /-/ apLen
     octoWrite (s!stack) (reverse ap) (s!sp2!val) -- 2x QUAD
     octoWrite (s!astack) (reverse roots) (s!sp2!val)

     tick

     octoRead (s!stack) (s!sp!val)
     octoRead (s!heap) (t!apAddr)
     octoRead (s!code) (t!funAddr)

     tick
  where
    t      = s!top!val
    ap     = s!heap!octoOut
    endHot = onlyFirst (map isEnd ap)
    apLen  = encode endHot ++ [low]
    roots  = map (mkAStkAddr . (t!apAddr /+/) . (`ofWidth` 4)) [0..7]
    newTop = select endHot ap

{- Optimisied variant of unwind (consumes 2 ticks instead of 3)

unwind s =
  do tick
     quadWrite (s!stack!fst) (take 4 ap') (take 4 mask) (s!sp!val /-/ apLen)
     quadWrite (s!stack!snd) (drop 4 ap') (drop 4 mask) (s!spPlus4 /-/ apLen)
     octoWrite (s!astack) (reverse roots) (s!sp!val /-/ unsigned 8)
     octoRead (s!heap) (newTop!apAddr)
     octoRead (s!code) (newTop!funAddr)
     s!sp <== s!sp!val /-/ apLen
     s!top <== newTop
     tick
  where
    t      = s!top!val
    ap     = s!heap!octoOut
    endHot = onlyFirst (map isEnd ap)
    apLen  = encode endHot ++ [low]
    roots  = map (mkAStkAddr . (t!apAddr /+/) . (`ofWidth` 4)) [0..7]
    mask   = tally (map isEnd ap)
    ap'    = rotateRight' endHot (reverse ap)
    newTop = last ap'

-}

-- Inst, for "instantiation", translates a node taken from code
-- memory so it is ready to be written onto the heap.

inst s node =
  pick [ node!isArg --> markEnd end arg
       , node!isAp  --> markEnd end (mkApNode ap)
       , (node!isFun <|> node!isInt) --> node
       ]
  where
    end = [node!isEnd]
    tops = s!stackBuf
    argNum = take 3 (node!getArg)
    arg = select (decode argNum) tops
    ap = s!base!val /+/ (node!getAp /-/ unsigned 1)

-- Unfold instantiates the body of the combinator (pointed to by top
-- of stack) onto the end of the heap, and in parallel, instantiates
-- thespine of the combinator onto the stack.

unfold s =
  do -- Buffer the arguments (i.e. top 8 items of stack memory)
     stackBufEn s <== [high]

     -- Read the second block of the function body
     octoRead (s!code) (s!top!val!funAddr /+/ unsigned 8)

     -- Initialise code pointer to third block of function body
     s!cp <== s!top!val!funAddr /+/ unsigned 16

     -- Save the original heap pointer
     s!base <== s!hp!val

     tick

     -- Update stack pointer & determine the root of the redex
     let rootPos = s!sp!val /+/ (numArgs ++ [low])
     let rootPos' = map flipflop rootPos
     s!sp <== rootPos /-/ (spineLen ++ [low])
     quadRead (s!astack!fst) rootPos

     -- Update top of stack & write spine to stack
     let firstBlock = rotl 1 block'
     s!top <== select (decode spineLen) firstBlock
     octoWrite (s!stack) (reverse firstBlock) (rootPos /-/ unsigned 8)

     -- Write first block of body to heap & update heap pointer
     octoWrite (s!heap) firstBlock (s!hp!val)
     s!wordsTodo <== bodyLen /-/ unsigned 7
     s!hp <== s!hp!val /+/ (minw bodyLen (7 `ofWidth` 10))

     -- Read next (second) block of function body & advance code pointer
     octoRead (s!code) (s!cp!val)
     s!cp <== s!cp!val /+/ unsigned 8
 
     tick

     -- Save the redex root & update address stack
     s!root <== s!astack!unitOut
     let addrs = map (\i -> mkAStkAddr (s!base!val /+/ (i `ofWidth` 5))) [0..7]
     octoWrite (s!astack) (reverse addrs) (rootPos' /-/ unsigned 8)
     let hpIncr = minw (s!wordsTodo!val) (8 `ofWidth` 10)

     -- Write rest of function body to the heap
     while (address 0 /</ (s!wordsTodo!val)) $
       do octoWrite (s!heap) block' (s!hp!val)
          octoRead (s!code) (s!cp!val)
          s!hp <== s!hp!val /+/ hpIncr
          s!cp <== s!cp!val /+/ unsigned 8
          s!wordsTodo <== s!wordsTodo!val /-/ unsigned 8
          tick

     -- Read new stack, heap, and code memories for next reduction
     let t = s!top!val
     octoRead (s!heap) (t!apAddr)
     quadRead (s!stack!fst) (s!sp!val)
     quadRead (s!stack!snd) (s!spPlus4)
     octoRead (s!code) (t!funAddr)

     tick

     -- Update root of redex
     onlyIf (s!root!val!head) $
       unitWrite (s!heap!fst) (mkApEndNode (s!base!val))
                 (s!root!val!rootAddr)
       --writeMem s (mkApEndNode (s!base!val))
       --            (s!root!val!rootAddr)
  where
    block = s!code!octoOut
    block' = map (inst s) block
    (numArgs, spineLen, bodyLen) = extractStart (block !! 0)
