module Red16.Collect where

import Lava
import Monad
import CircLib.Bit
import CircLib.Word
import CircLib.Common
import CircLib.RTL2
import Red16.ProcState
import Red16.Widths
import Red16.Mem
import Red16.Bytecode

{-

The garbage collector (collect) is a simple stop-and-copy two-space
collector, whose primary design goal is simplicity.

First, the collector copies applications pointed to by the stack from
the heap onto the GC heap (collectRoots).  Nodes on the GC heap are
then traversed and scavenged linearly (collectDeep).  In particular:

  * the linear traversal uses a pointer -- the "low water mark"
    pointer, initialised to 0 -- to scan though the GC heap.

  * when an application is encountered it is copied from the heap onto
    the end of the GC heap, resulting the the GC heap expanding.  The GC
    heap pointer can be thought of as a the "high water mark".

  * the traversal finishes when the low water mark reaches the high water
    mark.

  * when an application is scavenged, its location in the original heap
    is overwritten with a GC node pointing to its new location on the
    GC heap, so sharing is preserved.

Note that the collector is not recursive, so no stack is required to
traverse the graph on the heap.

Next, the address stack is updated by determining where each location
on the original heap has been moved to on the new heap.  Some
addresses may not have been garbage collected (unreferenced), in which
case the "address present" bit of the element of the address stack is
set to 0.

Finally, the contents of the GC heap,
which represents the original heap in compacted form, is copied
back to the heap (copyBack), and adjustments are made to restore the
Reduceron's invariants.

-}

collect s =
  do s!gc!gchp <== address 0
     s!gc!gcsp <== maxStackAddr
     s!sp <== s!sp!val /-/ unsigned 1
     tick

     unitWrite (s!stack!fst) (s!top!val) (s!sp!val)
     tick

     collectRoots s
     collectDeep s
     s!gc!gcsp <== maxStackAddr
     tick

     updateAStack s
     s!hp <== s!gc!gchp!val
     copyBack s
     unitRead (s!stack!fst) (s!sp!val)
     tick
     tick

     s!top <== s!stack!unitOut
     s!sp <== s!sp!val /+/ unsigned 1
     tick

     octoRead (s!stack) (s!sp!val)
     octoRead (s!heap) (s!top!val!apAddr)
     octoRead (s!code) (s!top!val!funAddr)
     tick
  where
    maxStackAddr = replicate stkAddrWidth high

-- collectRoots copies applications pointed to by the stack from the
-- heap onto the GC heap.

collectRoots s =
  while (s!sp!val |<=| s!gc!gcsp!val) $
    do unitRead (s!stack!fst) (s!gc!gcsp!val)
       tick
       tick

       onlyIf (s!stack!unitOut!isAp) $
         do copyAp s (s!stack!unitOut!getAp)
            unitWrite (s!stack!fst) (s!gc!newApAddr!val!mkApNode)
                      (s!gc!gcsp!val)

       s!gc!gcsp <== s!gc!gcsp!val /-/ unsigned 1
       tick

-- collectDeep copies unscavenged applications pointed to by the GC
-- heap from the heap onto the GC heap.

collectDeep s =
  do s!gc!collected <== address 0
     tick

     let toCollect = s!gc!gchp!val
     while (s!gc!collected!val |<| toCollect) $
       do node <- unitRead (s!gc!gcHeap!fst) (s!gc!collected!val)
          tick
          tick

          s!gc!wasEnd <== [node!isEnd]
          onlyIf (node!isAp) $
            do copyAp s (node!getAp)
               let new = s!gc!newApAddr!val!mkApNode
               let new' = markEnd (s!gc!wasEnd!val) new
               unitWrite (s!gc!gcHeap!fst) new' (s!gc!collected!val)

          s!gc!collected <== s!gc!collected!val /+/ unsigned 1
          tick

-- updateAStack updates the address stack such that addresses are
-- updated with their newly allocated addresses on the GC heap.

updateAStack s =
  while (s!sp!val |<=| s!gc!gcsp!val) $
    do unitRead (s!astack!fst) (s!gc!gcsp!val)
       tick

       onlyIf (s!astack!unitOut!head) $
         do unitRead (s!heap!fst) (s!astack!unitOut!rootAddr)
            tick
            tick

            let node = s!heap!unitOut
            let newRoot = (node!isGcNode : node!getAp) ++ [low, low]
            unitWrite (s!astack!fst) newRoot (s!gc!gcsp!val)

       s!gc!gcsp <== (s!gc!gcsp!val /-/ unsigned 1)
       tick

-- copyBack simply copies the contents of the GC heap to the heap.

copyBack s =
  do let gchp' = s!gc!gchp!val /-/ unsigned 1
     s!gc!gchp <== gchp'
     tick

     unitRead (s!gc!gcHeap!fst) (s!gc!gchp!val)

     tick

     while (s!gc!gchp!val!nandTree) $
       do unitRead (s!gc!gcHeap!fst) gchp'
          s!gc!gchp <== gchp'
          tick
          unitWrite (s!heap!fst) (s!gc!gcHeap!unitOut)
                    (s!gc!gchp!val /+/ unsigned 1)
  where
    nandTree = inv . tree (<&>)

-- copyAp copies an application from the heap to the GC heap, and
-- returns the address its new location on the GC heap.  The location
-- of the application on the original heap is overwritten with a GC
-- node pointing to the new location.  This allows an application that
-- has already been scavenged to be detected, and not copied again.

copyAp s src =
  do s!gc!apToCopy <== src
     s!gc!apPtr <== src
     s!gc!newApAddr <== s!gc!gchp!val
     node <- unitRead (s!heap!fst) src
     tick
     tick
     
     choose
       [ node!isGcNode -->
           do s!gc!newApAddr <== node!getAp
              tick

       , node!isGcNode!inv -->
           do doWhile (node!isEnd!inv) $
                do node <- unitRead (s!heap!fst) (s!gc!apPtr!val)
                   tick
                   s!gc!apPtr <== s!gc!apPtr!val /+/ unsigned 1
                   tick
                   s!gc!gchp <== s!gc!gchp!val /+/ unsigned 1
                   unitWrite (s!gc!gcHeap!fst) node (s!gc!gchp!val)
              unitWrite (s!heap!fst) (s!gc!newApAddr!val!mkGcNode)
                        (s!gc!apToCopy!val)
              tick
       ]
