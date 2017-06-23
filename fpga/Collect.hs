module Collect
  ( Collect
  , newCollect
  , collect
  , collecting
  , gcCount
  ) where

import Prelude hiding (Word, (.))
import Bytecode
import Lava
import Recipe
import Unistack as US
import CachingOctostack as OS
import Heap

data Collect =
  Collect {
    fromSpace     :: Heap HeapAddrN AppN
  , toSpace       :: Heap ToSpaceAddrN AppN
  , tsp           :: Reg ToSpaceAddrN
  , appToCopy     :: Reg AppN
  , child         :: Reg AtomN
  , origChild     :: Reg AtomN
  , newAtom0      :: Reg AtomN
  , newAtom1      :: Reg AtomN
  , newAtom2      :: Reg AtomN
  , usp           :: Reg UStackAddrN
  , uspNew        :: Reg UStackAddrN
  , ustack        :: Unistack UStackAddrN UpdateN
  , ustackSize    :: Reg UStackAddrN
  , stack         :: Octostack AtomN
  , savedTop      :: Reg AtomN
  , newTop        :: Sig AtomN
  , root          :: Reg AppN
  , copyChildProc :: Proc
  , collecting    :: Reg N1
  , gcCount       :: Reg N16
  }

newCollect heap updateStack valueStack sigNewTop =
  do toSpaceHeap   <- newHeap Width1 "gc_"
     tspReg        <- newReg
     appToCopyReg  <- newReg
     childReg      <- newReg
     origChildReg  <- newReg
     newAtom0Reg   <- newReg
     newAtom1Reg   <- newReg
     newAtom2Reg   <- newReg
     uspReg        <- newReg
     uspNewReg     <- newReg
     ustackSizeReg <- newReg
     savedTopReg   <- newReg
     rootReg       <- newReg
     copyChildP    <- newProc (copyChild childReg heap toSpaceHeap)
     collFlag      <- newReg
     gcCountReg    <- newReg

     return $ Collect {
                fromSpace     = heap
              , toSpace       = toSpaceHeap
              , tsp           = tspReg
              , appToCopy     = appToCopyReg
              , child         = childReg
              , origChild     = origChildReg
              , newAtom0      = newAtom0Reg
              , newAtom1      = newAtom1Reg
              , newAtom2      = newAtom2Reg
              , usp           = uspReg
              , uspNew        = uspNewReg
              , ustack        = updateStack
              , ustackSize    = ustackSizeReg
              , stack         = valueStack
              , savedTop      = savedTopReg
              , newTop        = sigNewTop
              , root          = rootReg
              , copyChildProc = copyChildP
              , collecting    = collFlag
              , gcCount       = gcCountReg
              }

{-

The garbage collector
---------------------

Algorithm:

  1. Dump the stack onto the heap.
  2. Copy the root of the graph onto to-space.
  3. Apply the copying collector.
  4. Update pointers on the update stack.
  5. Copy compacted graph back to the heap.

-}

collect top c = do c.collecting <== 1 -- for profiling
                   c.savedTop <== top
                   tick

                   c.toSpace.reset   -- added
                   c.gcCount <== c.gcCount.val + 1
                   tick

                   c.dumpStack       -- added
                   tick              -- added

                   c.toSpace.snocA (c.root.val) -- added 2
                   tick                         -- added 2

                   c.copy -- added 3
                   tick   -- added 3

                   c.updateUStack   -- added 5
                   tick             -- added 5

                   c.copyBack -- added 4
                   tick       -- added 4

                   c.collecting <== 0 -- for profiling
                   tick

                   -- c.newTop <== c.savedTop.val
                   c.fromSpace.lookupB 0
                   c.newTop <== makeAP low 0


{-

Dump stack
----------

Pops the stack onto the end of from-space.

-}

dumpStack c = doWhile (do c.savedTop <== makeAP low (c.fromSpace.Heap.size)
                          c.root <== app
                          c.fromSpace.snocA app
                          c.stack.update (minus n) 0 []
                          tick)
                      (c.stack.OS.size =/= 0)
  where
    n = (c.stack.OS.size |>=| 3) ? (3, c.stack.OS.size.vtake n2)
    app = makeApp n low low body
    body = (c.savedTop.val) +> (c.stack.tops.vtake n3)

minus :: Word N2 -> Word N4
minus n = b +> (a <#> b) +> (a <|> b) +> (a <|> b) +> vempty
  where (a, b) = (n `vat` n1, n `vat` n0)


{-

Copy
----

Copies all applications that are reachable from to-space onto the end
of to-space; if to-space contains only the root of the graph, then the
whole of the graph will be copied.  Things are slightly awkward
because we have to deal with up to 4 atoms at a time.

-}

copy c = do c.tsp <== 0
            tick

            while (c.tsp.val |<| c.toSpace.Heap.size) $do
                c.toSpace.lookupB (c.tsp.val)
                tick

                c.appToCopy <== c.toSpace.outputB
                c.child <== c.toSpace.outputB.atoms.vhead
                tick

                c.copyChildProc.call
                c.newAtom0 <== c.child.val

                iff (c.appToCopy.val.appArity |>=| 1) $do
                    c.child <== c.appToCopy.val.atoms `vat` n1
                    tick

                    c.copyChildProc.call
                    c.newAtom1 <== c.child.val

                iff (c.appToCopy.val.appArity |>=| 2) $do
                    c.child <== c.appToCopy.val.atoms `vat` n2
                    tick

                    c.copyChildProc.call
                    c.newAtom2 <== c.child.val

                iff (c.appToCopy.val.appArity |>=| 3 <|> c.appToCopy.val.hasAlts) $do
                    c.child <== c.appToCopy.val.atoms `vat` n3
                    tick

                    c.copyChildProc.call

                tick

                c.tsp <== c.tsp.val + 1
                let newAtoms = c.newAtom0.val
                            +> c.newAtom1.val
                            +> c.newAtom2.val
                            +> c.child.val
                            +> vempty
                    newApp   = makeApp (c.appToCopy.val.appArity)
                                       (c.appToCopy.val.isNF)
                                       (c.appToCopy.val.hasAlts)
                                       newAtoms
                 in
                   c.toSpace.updateA (c.tsp.val) newApp
                tick


{-

Copy child
----------

Copies the child of an atom (pointed to by the "child" field of the
Collect structure) onto the end of to-space, provided (1) the atom is
an AP, i.e. it is not a leaf atom, and (2) the child application atom
has not already been collected.  The old location of the child
application is overwritten with a pointer to its new location.
Indirections are followed, and shorted-out, hence the loop.  Upon
completion, the child field is updated to account for the child's
possible new location in to-space.

-}

copyChild child fromSpace toSpace =
  iff (child.val.isAP) $do
      fromSpace.lookupB (child.val.pointer)
      tick

      iff (doCopy) $do
          child <== makeAP (child.val.isShared) newAddr
          toSpace.snocA app
          fromSpace.updateA (child.val.pointer) (makeCollected newAddr)

      iff (inv doCopy) $do
        child <== app.atoms.vhead

      tick

  where
    app = fromSpace.outputB
    newAddr = toSpace.Heap.size <+ low
    doCopy = app.isCollected.inv <&> inv (app.isSingle <&> app.isNorm)

    isSingle app = app.appArity === 0 <&> app.hasAlts.inv
    isNorm app = app.atoms.vhead.isINT <|> app.atoms.vhead.isCON

{-
copyChild child fromSpace toSpace =
  child.val.isAP |>
    Seq [
      fromSpace.lookupB (child.val.pointer)
    , Tick
    , doCopy |>
        Seq [
          child <== makeAP (child.val.isShared) newAddr
        , toSpace.snocA app
        , fromSpace.updateA (child.val.pointer) (makeCollected newAddr)
        ]
    , doCopy.inv |>
        child <== app.atoms.vhead
    , Tick
    ]
  where
    app = fromSpace.outputB
    newAddr = toSpace.Heap.size <+ low
    doCopy = app.isCollected.inv <&> (app.isSingle.inv <|> app.isIndirection)

    isSingle app = app.appArity === 0 <&> app.hasAlts.inv
    isIndirection app = isSingle app <&> isAP (app.atoms.vhead)
-}

{-

Update U-stack
--------------

Traverse the update-stack and adjust heap pointers to the relocated
heap addresses.  Any update frames pointing to garbage are thrown
away.

-}

updateUStack c =
 do c.usp <== 3
    c.uspNew <== 3
    c.ustack.push 0
    tick

    c.ustack.push 0
    tick

    c.ustackSize <== c.ustack.US.size
    tick

    while (c.usp.val |<=| c.ustackSize.val) $do

        tick

        c.ustack.index (c.usp.val)
        tick

        c.fromSpace.lookupA ha
        tick

        c.usp <== c.usp.val + 1
        iff (c.fromSpace.outputA.isCollected) $do
            c.uspNew <== c.uspNew.val + 1
            let ha' = c.fromSpace.outputA.relocatedAddr in
              c.ustack.modify (c.uspNew.val) (makeUpdate sa' ha')

        tick

    tick

    c.ustack.setSize (c.uspNew.val - 1)
    tick
    tick
    tick

    c.ustack.pop
    tick

    c.ustack.pop
    tick

  where
    (sa, ha) = c.ustack.output.splitUpdate
    sa'      = delay 0 sa


{-
updateUStack c =
  Seq [
    c.usp <== 0
  , c.uspNew <== 0
  , c.ustackSize <== c.ustack.US.size
  , c.ustack.push 0
  , Tick
  , c.ustack.push 0
  , Tick
  , While (c.usp.val |<| c.ustackSize.val) $
      Seq [
        Tick
      , c.ustack.index (c.usp.val)
      , Tick
      , c.fromSpace.lookupA ha
      , Tick
      , c.usp <== c.usp.val + 1
      , c.fromSpace.outputA.isCollected |>
          Seq [
            c.uspNew <== c.uspNew.val + 1
          , let ha' = c.fromSpace.outputA.relocatedAddr in
              c.ustack.modify (c.uspNew.val) (makeUpdate sa ha')
          ]
      , c.fromSpace.outputA.isCollected.inv |> c.ustack.pop
      ]
  , Tick
  , Tick
  , c.ustack.pop
  , Tick
  , c.ustack.pop
  , Tick
  ]
  where
    (sa, ha) = c.ustack.output.splitUpdate
-}

{-

Copy back
---------

Copies the compacted graph from to-space back to from-space.

-}

copyBack c =
  do c.fromSpace.reset
     c.tsp <== 0
     tick

     while (c.tsp.val |<| c.toSpace.Heap.size) $do
        c.toSpace.lookupA (c.tsp.val)
        c.tsp <== c.tsp.val + 1
        tick

        c.fromSpace.snocA (c.toSpace.outputA)
        tick
