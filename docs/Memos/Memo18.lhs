==========================
REDUCERON MEMO 18
Spineless versus Spinefull
Matthew N, 23 January 2008
==========================

In Memo 15, we presented a semantics for a spineless Reduceron and, at
the end, some figures suggesting that it will be more efficient than
the existing Reduceron.  However, we did not intend to suggest that the
efficiency improvement is entirely due to moving from a spinefull
machine to a spineless one.  The real cause of the speed-up is surely
the use of memory-aligned applications, which is not specific to a
spineless machine.  Peyton Jones states that the spineless G-machine
is more efficient that the original (spinefull) G-machine, but he is
concerned with the PC architecture.  If the heap and stack can be
accessed in parallel (as in the Reduceron), then the spine can be
written to both at the same time, absorbing the main inefficiency of a
spinefull machine.

So the question is: which is better, a spineless or a spinefull
Reduceron?  This memo presents a semantics and a compiler for
spinefull Reduceron with memory-aligned applications, discusses its
strengths and weaknesses compared to the spineless Reduceron, and
finally presents some performance figures.

> import List
> import System.IO.Unsafe(unsafePerformIO)

Semantics
=========

> type Heap = [App]
> type HeapAddr = Int

> type App = [Node]

The first difference from the spineless machine is that application
and argument nodes do not contain a 'possibly-shared' flag.  However,
it does seem possible to utilise such sharing information in a
spinefull machine, and we comment on this below.

> data Node =
>     APP HeapAddr
>   | INT Int
>   | FUN Arity [Instr]
>   | PRI PrimOp
>   | ARG Int

> type Shared = Bool

> type Arity = Int

> type PrimOp = String

> data Instr =
>     MKAP App
>   | PUSH App

> type Stack = [Node]
> type StackAddr = Int

The update stack is a still list of updates like in the spineless
machine.  But the update stack now stores the heap address of every
node on the update stack.  The update stack in the spineless machine
contained only update records for the possibly-shared applications
currently under evaluation.

> type UStack = [Update]

An update record now contains the heap address of the application to
update, and an offset within that application. 

> data Update = Update HeapAddr Int

The offset is needed to correctly update an over-saturated
application.  For example, if the application 'f a b' at address 'n'
is unwound onto the stack, then the stack and address stack (which
both grow downwards) look as follows.

  STACK     USTACK
  b         n,3
  a         n,2
  f         n,1

If 'f a' reduces to 'z', we must update the 'f a b' at address 'n'
with 'z b'.  This can be done efficiently if the nodes of an
application are stored in reverse order on the heap.

The spinefull evaluator differs in several places from the spinless
one:

  1. To unwind an application of length 'n', 'n' nodes must be written
     both onto the node and address stacks.  To do this efficiently,
     we need a wide update stack as well as a wide node stack.  In
     the spineless machine, we didn't need a wide update stack.

  2. No "update-check" is required.  Instead, an update is performed
     on every reduction, including primitive and non-primitive
     function application.

  3. When executing a PUSH instruction, an update is performed,
     utilising one of the ports on the heap. If the number of
     instructions to execute is even, then both ports of the heap are
     in use on the final clock-cycle, so we must delay for a clock-cycle
     to read the next application.  In the spineless machine, PUSH
     instructions never access the heap.

  4. Although the spine can be written to the heap and stack in
     parallel, heap space is wasted when unnecessarily writing the
     spine to the heap.

  5. Like in the spineless machine, updating is done by overwriting
     the redex with the result (not with an indirection).  However,
     there is a special case where an indirection is required: when an
     application contained within an over-saturated application
     has been reduced.  Such an update requires two heap accesses: one
     to write the indirection, and one to write the application.

If 'possibly-shared' applications were implemented, as in the
spineless machine, the spinefull machine could in sometimes avoid
updating, freeing up the heap for other uses.

> eval :: Stack -> UStack -> Heap -> Int
> eval (APP addr:s) (_:u) h = eval (ap ++ s) (us ++ u) h
>   where ap = h !! addr
>         us = map (Update addr) [1..length ap]
> eval [INT i] u h = i
> eval (INT i:e:s) u h = eval (e:INT i:s) u h
> eval (PRI p:s) u h = eval s' u' h'
>   where res = applyPrim p s
>         (s', u', h') = performUpdate (drop 2 s) (drop 3 u) h (u !! 2) [res]
> eval (FUN arity code:s) u h = eval s' u'' h'
>    where (args, rest) = splitAt arity s
>          root = u !! arity
>          u' = drop (arity+1) u
>          (s', u'', h') = foldl (exec args (length h) root) (rest, u', h) code

> exec args base root (s, u, h) (MKAP app) = (s, u, h')
>   where h' = h ++ [map (inst args base) app]
> exec args base root (s, u, h) (PUSH app) = performUpdate s u h root app'
>   where app' = map (inst args base) app

> performUpdate s u h (Update a i) app
>   | length app <= i = let addrs = map (Update a) [1..length app] in
>       (app ++ s, addrs ++ u, update a app h)
>   | otherwise       = let addrs = map (Update (length h)) [1..length app] in
>       (app ++ s, addrs ++ u, update a [APP (length h)] h ++ [app])

> update :: Int -> a -> [a] -> [a]
> update i a xs = [if i == j then a else x | (j, x) <- zip [0..] xs]

> inst :: [Node] -> HeapAddr -> Node -> Node
> inst args base (APP offset) = APP (base + offset)
> inst args base (ARG index)  = args !! index
> inst args base n            = n

> applyPrim :: PrimOp -> Stack -> Node
> applyPrim "(+)"     (INT a:INT b:s) = INT (a + b)
> applyPrim "(-)"     (INT a:INT b:s) = INT (a - b)
> applyPrim "(==)"    (INT a:INT b:s) = bool (a == b)
> applyPrim "(/=)"    (INT a:INT b:s) = bool (a /= b)
> applyPrim "(<=)"    (INT a:INT b:s) = bool (a <= b)
> applyPrim "emit"    (INT a:k:s)     = emitStr [toEnum a] k
> applyPrim "emitInt" (INT a:k:s)     = emitStr (show a) k

> bool :: Bool -> Node
> bool False = FUN 2 [PUSH [ARG 0]]
> bool True  = FUN 2 [PUSH [ARG 1]]

> emitStr :: String -> a -> a
> emitStr s k = unsafePerformIO (putStr s) `seq` k

Compiler
========

> type Prog = [Defn]

> type Defn = (Id, Int, [Bind], Exp)

> type Id = String

> type Bind = (Id, Exp)

> data Exp =
>     Apply [Exp]
>   | Fun Id
>   | Prim Id
>   | Var Id
>   | Arg Shared Int
>   | Int Int

> compile :: Prog -> Node
> compile p = head [FUN arity code | ("main", arity, code) <- p']
>   where p' = [(f, arity, comp p' bs e) | (f, arity, bs, e) <- p]

> comp p bs e = compSpine p binds code e
>   where (vs, es) = unzip bs
>         binds = zip vs ns
>         (code, ns) = mapAccumL (compExp p binds) [] es

> compSpine p binds code (Apply es) = fst (compApp p binds True code es)
> compSpine p binds code e = compSpine p binds code (Apply [e])

> compExp p binds code (Int i) = (code, INT i)
> compExp p binds code (Var v) = (code, n)
>   where n = head [n | (w, n) <- binds, v == w]
> compExp p binds code (Arg shared i) = (code, ARG i)
> compExp p binds code (Prim f) = (code, PRI f)
> compExp p binds code (Fun f) = (code, n)
>   where n = head [FUN arity is | (g, arity, is) <- p, f == g]
> compExp p binds code (Apply es) = compApp p binds False code es

> compApp p binds spine code es =
>   (code' ++ [if spine then PUSH ns else MKAP ns], APP (length code'))
>   where (code', ns) = mapAccumL (compExp p binds) code es

> run :: Prog -> Int
> run p = eval [compile p] [Update 0 1] [undefined]

Estimated performance
=====================

Number of clock ticks taken by spineless and spinefull
implementations on a range of benchmarks:

  +-----------+------------+-----------+
  | PROGRAM   | SPINELESS  | SPINEFULL |
  +-----------+------------+-----------+
  | Fib       | 129666     | 158955    |
  | Sudoku    | 97397      | 133665    |
  | OrdList   | 455078     | 531051    |
  | Queens    | 443056     | 473734    |
  | Queens2   | 373775     | 392223    |
  | PermSort  | 353090     | 358934    |
  | MSS       | 468804     | 516212    |
  | Clausify  | 639097     | 793645    |
  | While     | 453986     | 512452    |
  +-----------+------------+-----------+

Conclusion
==========

The spineless Reduceron appears to have a few small advantages over a
spinefull one.

  1. It is slightly faster.
  2. It uses less heap space.
  3. It doesn't require sometimes introducing indirections when
     updating a redex.
  4. Its update stack is simpler; only one element ever needs to be
     pushed or popped at a time.  Its update stack also doesn't grow as
     large.
  5. It is more modular, as it separates updating from application,
     making application quite a bit simpler.
