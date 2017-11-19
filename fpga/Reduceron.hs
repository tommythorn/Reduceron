module Reduceron where

import Prelude hiding (Word, (.))
import Lava
import Recipe
import Bytecode
import Unistack as US
import CachingOctostack as OS
import RegFile
import Code
import Heap
import Collect

{-

The Reduceron structure
-----------------------

Contains:

  * The newly computed top element of the value stack (newTop), and
    this value delayed by one clock-cycle (top).

  * The current reduction state of the Reduceron (see below for a
    list of the different states).

  * The signal dashTopN which allows the top N elements of the stack
    to be dashed (i.e. marked as possibly-shared).  See the section
    on updating for more details.

  * Five memories: three stacks (value, update, and case-alternative
    stacks), a heap, and code memory.

-}

data Reduceron =
  Reduceron {
    top        :: Atom
  , newTop     :: Sig AtomN
  , state      :: State
  , dashTopN   :: Sig N3
  , vstack     :: Octostack AtomN
  , ustack     :: Unistack UStackAddrN UpdateN
  , astack     :: Unistack LStackAddrN FunAddrN
  , heap       :: Heap HeapAddrN AppN
  , code       :: Code TemplateN
  , regFile1   :: RegFile N4 AtomN
  , regFile2   :: RegFile N4 AtomN
  , result     :: Reg AtomN
  , collector  :: Collect
  , ioAddr     :: Sig NumberN
  , ioWriteData:: Sig NumberN
  , ioWrite    :: Sig N1
  , ioRead     :: Sig N1
  , ioWait          :: Bit
  , ioReadDataValid :: Bit
  , ioReadData      :: Word NumberN
  }

newReduceron :: [Integer] -> New Reduceron
newReduceron program =
  do nt   <- newSig
     da   <- newSig
     v    <- newOctostack "vstack_" (dashN (da.val))
     u    <- newUnistack Width9
     a    <- newUnistack Width9
     h    <- newHeap Width1 ""
     c    <- newCode program (fetch (nt.val) (a.newTopElem)) Width18
     res  <- newReg
     col  <- newCollect h u v nt
     rf1  <- newRegFile n4
     rf2  <- newRegFile n4

     let du = updateCheck (nt.val) (v.OS.newSize) (u.newTopElem)
                <&> (u.US.newSize.orG)
     let next = nextState (nt.val) du (v.OS.newSize) (h.Heap.size)
                  (col.collecting.val.vhead)

     ioAddr      <- newSig
     ioWrite     <- newSig
     ioWriteData <- newSig
     ioRead      <- newSig

     return $ Reduceron {
                top        = delay 0 (nt.val)
              , newTop     = nt
              , state      = delay 0 next
              , dashTopN   = da
              , vstack     = v
              , ustack     = u
              , astack     = a
              , heap       = h
              , code       = c
              , regFile1   = rf1
              , regFile2   = rf2
              , result     = res
              , collector  = col
              , ioAddr     = ioAddr
              , ioWrite    = ioWrite
              , ioWriteData= ioWriteData
              , ioRead     = ioRead
              , ioWait          = name "ioWait"
              , ioReadDataValid = name "ioReadDataValid"
              , ioReadData      = nameWord "ioReadData"
              }

{-

Top-level dispatch loop
-----------------------

-}

dispatch :: Reduceron -> Recipe
dispatch r = do tick
                while (r.state.isHaltState.inv) $do
                     r.unwind
                     r.performUpdate
                     r.prim
                     r.unfold
                     r.fetchApp
                     iff (isGCState (r.state)) $ r.collector.collect (r.top)
                     tick
                r.result <== r.top
                tick
  where
    stop = r.state.orG.inv

{-

State Machine
-------------

The Reduceron has seven reduction states:

  +---------+----------+
  | State   | Encoding |
  +---------+----------+
  | Unwind  | 1000000  |
  | Update  | 0100000  |
  | Swap    | 0010000  |
  | Prim    | 0001000  |
  | Unfold  | 0000100  |
  | GC      | 0000010  |
  | Halt    | 0000001  |
  +---------+----------+

The next state does not depend on the current state; rather, it is
always determined by the top element of the value stack.

-}

type StateN = N7
type State  = Word StateN

isUnwindState :: State -> Bit
isUnwindState s = s `vat` n0

isUpdateState :: State -> Bit
isUpdateState s = s `vat` n1

isSwapState :: State -> Bit
isSwapState s = s `vat` n2

isPrimState :: State -> Bit
isPrimState s = s `vat` n3

isUnfoldState :: State -> Bit
isUnfoldState s = s `vat` n4

isGCState :: State -> Bit
isGCState s = s `vat` n5

isHaltState :: State -> Bit
isHaltState s = s `vat` n6

initState :: State
initState = low +> low +> low +> low +> high +> low +> low +> vempty

nextState top doUpdate stackSize heapSize collecting =
  unwind +> update +> swap +> low +> unfold +> gc +> halt +> vempty
  where
    unwind = inv collecting <&> isAP top
    update = inv collecting <&> inv unwind <&> doUpdate
    swap   = inv collecting <&> inv doUpdate <&> isINT top <&> orG stackSize
    unfold = inv collecting <&> inv gc <&> inv doUpdate <&>
              (isCON top <|> isFUN top)
    gc     = collecting <|> (inv doUpdate <&> isHeapFull heapSize <&>
              ((isFUN top <&> funFirst top) <|> isCON top))
    halt   = inv collecting <&> inv doUpdate <&> isINT top <&>
               inv (orG stackSize)

isHeapFull :: HeapAddr -> Bit
isHeapFull n = andG $ vtake n4 $ vreverse n

{-

Update check
------------

Determines whether or not an update needs to be performed.

-}

updateCheck :: Atom -> StackAddr -> Update -> Bit
updateCheck top stackSize update =
  vlast diff <|> (vextend low (arity top) |>| vinit diff)
  where
    (sa, ha)    = splitUpdate update
    Signed diff = Signed (stackSize <+ low) - Signed (sa <+ low)

{-

Template and application pre-fetching
-------------------------------------

The fetch function determines the location of the needed template in
code memory.  The fetchApp function pre-fetches an application from
the heap, in preperation for the unwinding operation.

-}

fetch :: Atom -> FunAddr -> FunAddr
fetch top alts = isCON top ? (jump, funAddr top)
  where jump = alts + conIndex top

fetchApp :: Reduceron -> Recipe
fetchApp r = iff cond $ r.heap.lookupB (r.newTop.val.pointer)
  where cond = isUnwindState (r.state)
           <|> isSwapState (r.state)
           <|> (isUnfoldState (r.state) <&> inv (r.code.templateInstAtoms2))

{-

Unwinding
---------

An application is read from the heap and pushed onto the stack.  If
the application is possibly-shared, then it is first "dashed".  If the
application is updatable (i.e. possibly-shared and not a normal form)
then an update is pushed onto the update stack.  If the application
contains case alternatives, then the base address of the case
alternatives is push onto the case stack.

Given the arity of the application, the auxiliary function unwindMask
determines the push mask, needed for the Octostack's update operation.

  +-------+-----------+
  | Arity | Push mask |
  +-------+-----------+
  | 00    | 00000000  |
  | 01    | 10000000  |
  | 10    | 11000000  |
  | 11    | 11100000  |
  +-------+-----------+

-}

unwindMask :: Word N2 -> Word N8
unwindMask n = (a <|> b) +> a +> (a <&> b) +> vecOf low
  where (a, b) = (n `vat` n1, n `vat` n0)

unwind :: Reduceron -> Recipe
unwind r = iff (isUnwindState (r.state)) $do
               r.newTop <== vhead as
               r.vstack.update (n <+ low <+ low) (unwindMask n) (velems $ vtail as)
               iff (app.hasAlts) $ r.astack.push (app.alts)
               iff (up)          $ r.ustack.push (makeUpdate (r.vstack.OS.size) (r.top.pointer))
  where
    app = r.heap.outputB
    n   = app.appArity
    as  = vmap (dash sh) (app.atoms)
    sh  = r.top.isShared
    up  = sh <&> inv (app.isNF)

{-

Updating
--------

If functions can take no more than 7 arguments, then the maximum
application length of a normal form is 7.  Since heap cells store
applications of maximum length 4, a normal form must split in two
before being written to the heap.  To illustrate, if the application
at address A is to be overwritten with NF, then NF is split into APP1
and APP2 as follows.

        +----+----+----+----+----+----+----+
  NF:   | f  | a1 | a2 | a3 | a4 | a5 | a6 |
        +----+----+----+----+----+----+----+

        +----+----+----+----+
  APP1: | f  | a1 | a2 | a3 |
        +----+----+----+----+

        +----+----+----+----+
  APP2: | HP | a4 | a5 | a6 |
        +----+----+----+----+

  (HP is a pointer to the next free cell on the heap.)

Then APP1 is written to address HP and APP2 is written to address A.

Optimisation: if length of NF is <= 4 then APP1 is written to address
A, and APP2 ignored.

Given the length of the normal form, the following relation defines
the correct arities for APP1 and APP2.

  +----------+------------+------------+
  | NF Arity | APP1 Arity | APP2 Arity |
  +----------+------------+------------+
  | 000      | 00         | 00         |
  | 001      | 01         | 00         |
  | 010      | 10         | 00         |
  | 011      | 11         | 00         |
  | 100      | 11         | 01         |
  | 101      | 11         | 10         |
  | 110      | 11         | 11         |
  | 111      | XX         | XX         |
  +----------+------------+------------+

  (Most significant bits first.)

Other tasks carried out by the update operation:

  * The top update is popped of the update stack.

  * The top n-1 stack elements, where n is the length of NF, are
    dashed.  This is because the process of writing NF to the heap may
    duplicate a reference to a previously unshared application.

-}

performUpdate :: Reduceron -> Recipe
performUpdate r = iff (isUpdateState (r.state)) $do
                      r.newTop <== r.top
                      r.ustack.US.pop
                      iff (r.vstack.OS.size |>=| sa) $do
                          r.dashTopN <== n
                          iff long $ r.heap.snocA (makeApp a1 high low app1)
                          r.heap.updateB ha (long ? ( makeApp a2 high low app2
                                                    , makeApp a1 high low app1
                                                    ) )
  where
    (sa, ha) = splitUpdate (r.ustack.topElem)
    n        = vtake n3 (r.vstack.OS.size - sa)
    ts       = vmap (dash high) (r.vstack.OS.tops)
    app1     = r.top +> vtake n3 ts
    app2     = makeAP low (r.heap.Heap.size) +> vtake n3 (vdrop n3 ts)
    (a1, a2) = splitArity n
    long     = n `vat` n2

splitArity :: Word N3 -> (Word N2, Word N2)
splitArity n = (app1Arity, app2Arity)
  where
    (a, b, c) = (n `vat` n2, n `vat` n1, n `vat` n0)
    app1Arity = (a <|> c) +> (a <|> b) +> vempty
    app2Arity = (a <&> inv c) +> (a <&> (b <|> c)) +> vempty

dashN :: Word N3 -> Vec N8 Atom -> Vec N8 Atom
dashN n tops = Vec (zipWith dash xs (velems tops))
  where xs = tally (velems n)

{-

Swapping
--------

Swaps the top two stack elements.

-}

swap :: Reduceron -> Recipe
swap r = iff (isSwapState (r.state)) $do
             r.newTop <== r.vstack.OS.tops.vhead
             r.vstack.OS.update 0 (high +> 0) [r.top]

{-

Primitive application
---------------------

Pops the two arguments from the stack, and overwrites the top stack
element with the result of the primitve applied to those arguments, as
computed by the arithmetic and logic unit (ALU).  Implementation
according to Memo 40.

-}

prim :: Reduceron -> Recipe
prim r = iff (isSwapState (r.state)) $do

             iff (ready) $do
                 r.newTop <== result
                 iff (inv st32) $ r.vstack.update (-2) 0 []

                 -- Handle IO primitives
                 iff (st32) $do r.vstack.update (-3) 0 []
                                r.ioWrite     <== 1
                                r.ioAddr      <== sw ? (arg2.intValue, arg1.intValue)
                                r.ioWriteData <== sw ? (arg1.intValue, arg2.intValue)

             iff (inv ready) $do
                 r.newTop <== arg2
                 r.vstack.update 0 3 [pr.invSwapBit, arg1]

  where
    pr = r.vstack.OS.tops `vat` n0
    st32 = isST32 pr
    sw = pr.getSwapBit
    arg1 = r.top
    arg2 = r.vstack.OS.tops `vat` n1
    ready = arg2.isINT
    result0 = alu pr (arg1.intValue) (arg2.intValue)
    result1 = alu pr (arg2.intValue) (arg1.intValue)
    result = st32 ? (r.vstack.OS.tops `vat` n2, sw ? (result1, result0))

alu f a b =
  pickG
    [ isADD f --> makeINT (a+b)
    , isSUB f --> makeINT (a-b)
    , (isNEQ f <|> isEQ f) --> bool ((a === b) <#> isNEQ f)
    , isLEQ f --> bool (Signed a |<=| Signed b)
    , isAND f --> makeINT (vzipWith (<&>) a b)
    ]

{-

Unfolding
---------

Instantiates a template from code memory onto the heap and stack.

-}

unfold :: Reduceron -> Recipe
unfold r = iff (isUnfoldState (r.state)) $do
               r.newTop <== t.templateTop
               iff (r.top.isCON)          $ r.astack.US.pop
               iff (t.templateInstApp1)   $ r.heap.snocA' (t.templateApp1)
               iff (t.templateInstAtoms2) $ r.heap.snocB' (t.templateApp2)
               r.vstack.OS.update (t.templateOffset)
                                  (t.templatePushMask <+ low <+ low <+ low)
                                  (t.templateApp2Atoms.velems)
               iff (t.templatePushAlts)   $ r.astack.US.push (t.templateAlts)

               iff (t.templateIsApp1Prim) $ r.regFile1.assign (t.templateDestReg1) cand1
               iff (t.templateIsApp2Prim) $ r.regFile2.assign (t.templateDestReg2) cand2

               iff (hincA <|> hincB)      $ r.heap.advanceA
               iff (hincB)                $ r.heap.advanceB

  where
    t = mapTemplate (inst r) (r.code)

    hincA = (t.templateIsApp1Prim) ? (inv valid1, t.templateInstApp1)
    hincB = (t.templateIsApp2Prim) ? (inv valid2, t.templateInstAtoms2)

    -- PRS
    cand1 = valid1 ? (result1, ptr1)
    cand2 = valid2 ? (result2, ptr2)
    ptr1 = makeAP low (r.heap.Heap.size)
    ptr2 = makeAP low (r.heap.Heap.size1)
    valid1 = arg1A.isINT <&> arg1B.isINT
    valid2 = arg2A.isINT <&> arg2B.isINT
    result1 = alu p1 (arg1A.intValue) (arg1B.intValue)
    result2 = alu p2 (arg2A.intValue) (arg2B.intValue)
    arg1A = t.templateApp1.atoms `vat` n0
    arg1B = t.templateApp1.atoms `vat` n2
    arg2A = t.templateApp2.atoms `vat` n0
    arg2B = t.templateApp2.atoms `vat` n2
    p1 = r.code.templateApp1.atoms `vat` n1
    p2 = r.code.templateApp2.atoms `vat` n1


inst :: Reduceron -> Atom -> Atom
inst r a =
  pickG
    [ a.isARG   --> dash (a.isArgShared) x
    , a.isREG   --> dash (a.isRegShared) y
    , a.isAP    --> makeAP (a.isShared) (r.heap.Heap.size + a.pointer)
    , otherwise --> a
    ]
  where
    otherwise = inv (a.isARG <|> a.isREG <|> a.isAP)
    x         = selectG (a.argIndex.velems.take 7)
                        (r.vstack.OS.tops.velems.take 7)
    y         = selectG (a.regIndex.velems)
                        (undeal (r.regFile1.elems.velems)
                                (r.regFile2.elems.velems))

undeal xs [] = xs
undeal [] ys = ys
undeal (x:xs) (y:ys) = x:y:undeal xs ys
