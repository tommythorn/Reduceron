=============================
REDUCERON MEMO 15
Towards a spineless Reduceron
Matthew N, 13 January 2009
=============================

We define the semantics of a spineless template-instantiation machine
with vectored applications, and its compiler.  We distinguish between
non-shared and possibly-shared applications by a dynamic analysis,
allowing updates of definitly non-shared applications to be avoided.
We measure the number of updates saved accross a range of programs,
and estimate the performance of a hardware implementation of the
machine.  Finally, we point out the main ways in which we intend to
improve the machine and compiler.

We begin with a few imports.

> import List
> import System.IO.Unsafe(unsafePerformIO)

Use of 'unsafePerformIO' is confined to the implementation of the
'emit' and 'emitInt' primitives of F-lite.

Semantics
---------

The heap is modelled as a list of applications, and can be indexed by
a heap-address.

> type Heap = [App]
> type HeapAddr = Int

An application is a list of nodes.

> type App = [Node]

Typically, function application is represented as a binary operator.
A list is a useful generalisation because we can make efficient use of
a machine with a wide word size.

A node is an application pointer, an integer, a function,
a primitive function, or an argument.

> data Node =
>     APP Shared HeapAddr
>   | INT Int
>   | FUN Arity [Instr]
>   | PRI PrimOp
>   | ARG Shared Int
>   deriving Show

An application may be unshared or possibly-shared, represented by
False and True respectively.

> type Shared = Bool

Similarly, arguments can be unshared or shared, depending on how many
times they are referenced in the body of a function.  Argument nodes
should never appear on the heap, only in template memory.

> type Arity = Int

Function nodes contain an arity and a list of instructions to execute
when the function is applied.  So template memory is implicit.  An
instruction either builds an application on the heap, or pushes an
application on the stack.

> data Instr =
>     MKAP App
>   | PUSH App
>   deriving Show

For example, the function 's' defined by

  s f g x = f x (g x)

would be represented by the following node.

  FUN 3 [ MKAP [ARG False 1, ARG True 2]
        , PUSH [ARG False 0, ARG True 2, APP False 0]
        ]

The application node 'APP False 0' is a relative address, referring
to the application at address 'hp+0' where 'hp' is the value of the
heap pointer before entering the code for 's'.

The only difference between an application made using MKAP and one
made using PUSH is that the latter application represents the spine of
the function body.  In a spineless machine, this application need only
be built on the stack, not the heap.

In the compilation scheme presented below, function nodes always
contain exactly one PUSH instruction.  However, in a machine with a
finite application size, it may be useful to perform several PUSHes.

For efficiency, it is useful to perform PUSHes after MKAPs as they do
not access the heap.  Consequently, if the new top-of-stack is an
application node, then the application can be pre-fetched from the
heap, saving a clock-cycle.  What's more serious is that performing
PUSHes before MKAPs would destroy the stack before the MKAPs have
fetched the necessary arguments.

F-lite supports the following 7 primitive operations: (+), (-), (==),
(/=), (<=), emit, and emitInt.

> type PrimOp = String

The stack is modelled as a list of nodes, and can be indexed by a
stack-address.

> type Stack = [Node]
> type StackAddr = Int

A spineless machine also contains an update-stack.

> type UStack = [Update]

An update is a stack-address/heap-address pair.  When a function
demands arguments beyond the stack-address of the top update, the
value at the corresponding heap-address is updated.  See below for
details.

> data Update = Update StackAddr HeapAddr

Here is a small-step operational semantics for the machine.  It
operates over a triple containing the stack, the update-stack and the
heap.  It assumes that the expression to be evaluated is of type
integer.

> eval :: Stack -> UStack -> Heap -> Int
> eval (APP sh addr:s) u h = eval (dashApp sh (h !! addr) ++ s) u' h
>   where u' = if sh then Update (length s+1) addr:u else u
> eval s (Update saddr haddr:u) h
>   | updateCheck s saddr = eval s' u h'
>   where (s', h') = performUpdate s h saddr haddr
> eval [INT i] u h = i
> eval (INT i:a:s) u h = eval (a:INT i:s) u h
> eval (PRI p:s) u h = eval (applyPrim p s) u h
> eval (FUN arity code:s) u h = eval s' u h'
>    where (args, rest) = splitAt arity s
>          (s', h') = foldl (exec args (length h)) (rest, h) code

The following sections elaborate on each of the rewrite rules.

Unfolding
---------

To unfold a function, we execute its instructions.  To do this, we
need to know the arguments to the function, the heap pointer (to
convert relative addresses in template-memory into absolute ones on
the heap), the argument-less stack (on which the spine application
will pushed) and the heap (where the other applications will be
written to).  During execution, the stack and the heap are modified.

> exec :: [Node] -> HeapAddr -> (Stack, Heap) -> Instr -> (Stack, Heap)
> exec args base (s, h) (MKAP app) = (s, h')
>   where h' = h ++ [map (inst args base) app]
> exec args base (s, h) (PUSH app) = (map (inst args base) app ++ s, h)

> inst :: [Node] -> HeapAddr -> Node -> Node
> inst args base (APP shared offset) = APP shared (base + offset)
> inst args base (ARG shared index)  = dash shared (args !! index)
> inst args base n                   = n

When substituting for a shared argument we must ensure that, if the
substituted value is an application, then it is marked as shared.
Peyton Jones calls this operation "dashing".

> dash :: Bool -> Node -> Node
> dash True (APP _ addr) = APP True addr
> dash _    n            = n

Unwinding
---------

Unwinding an application is not simply a case of copying it from the
heap to the stack.  If it is a shared application, we must dash each
node in order to propagate the sharing information.

> dashApp :: Bool -> App -> App
> dashApp b = map (dash b)

Furthermore, if it is a shared application, we must push an update on
the update-stack.  This allows us to detect when evaluation of the
application has reached head normal form, and tells us the address to
write the result to.

Updating
--------

An update-check is performed before every reduction (but not before
unwinding).  The check returns true if the arity of the function to
reduce is larger that the difference between the current stack-pointer
and the stack-address of the top update on the update-stack.

> updateCheck :: Stack -> StackAddr -> Bool
> updateCheck s@(top:_) saddr = arity top > length s - saddr

> arity :: Node -> Int
> arity (FUN arity code) = arity
> arity (PRI p) = 2
> arity (INT i) = 1

All primitives have an arity of two.  Perhaps surprisingly, an integer
has an arity of one, but indeed it is useful to treat integers as
unary functions as we'll see.

To perform the update, we compute the length of the normal-form, read
it from the stack, and write it to the heap.  Quite subtly, we must
also dash the normal-form on the stack, because the process of writing
it to the heap could duplicate an otherwise unshared reference.

> performUpdate :: Stack -> Heap -> StackAddr -> HeapAddr -> (Stack, Heap)
> performUpdate s h saddr haddr = (app ++ drop n s, update haddr app h)
>   where n = 1 + length s - saddr
>         app = dashApp True (take n s)

> update :: Int -> a -> [a] -> [a]
> update i a xs = [if i == j then a else x | (j, x) <- zip [0..] xs]

Primitives
----------

To force evaluation of the arguments to strict binary-arithmetic
primitives, primitive applications occuring in the F-lite program are
transformed by the rule

  a x y   -->   y (x a)

where 'a' is an arithmetic primitive and 'x' and 'y' are expressions.
Emit primitives, which are binary operations requiring only their
first arguments to be evaluated, are handled by the similar rule

  e x k   -->   x e k

where 'e' is an emit primitive and 'x' and 'k' are expressions.

These transformations treat integers as unary functions.
Specifically, an integer 'i' is reduced by the following rule

  i k     -->   k i

which corresponds to a swap operation in the reduction machine.

Applying a primitive function is now rather straightforward because
we know the arguments must be evaluated.

> applyPrim :: PrimOp -> Stack -> Stack
> applyPrim "(+)"     (INT a:INT b:s) = INT (a + b):s
> applyPrim "(-)"     (INT a:INT b:s) = INT (a - b):s
> applyPrim "(==)"    (INT a:INT b:s) = bool (a == b):s
> applyPrim "(/=)"    (INT a:INT b:s) = bool (a /= b):s
> applyPrim "(<=)"    (INT a:INT b:s) = bool (a <= b):s
> applyPrim "emit"    (INT a:k:s)     = emitStr [toEnum a] k:s
> applyPrim "emitInt" (INT a:k:s)     = emitStr (show a) k:s

We assume that data constructors and case expressions have been
compiled using Jansen's encoding.  So the constructors 'True' and
'False' are represented by the following nodes.

> bool :: Bool -> Node
> bool False = FUN 2 [PUSH [ARG False 0]]
> bool True  = FUN 2 [PUSH [ARG False 1]]

The 'emitStr' function takes two arguments and returns its second.  It
has the side-effect of printing its first argument.

> emitStr :: String -> a -> a
> emitStr s k = unsafePerformIO (putStr s) `seq` k

This completes the small-step semantics.  Now we define a compiler for
the machine.

Compilation
-----------

Programs contain function definitions,

> type Prog = [Defn]

and a function definition consists of a function identifier, its
arity, a list of let-bindings, and its body.

> type Defn = (Id, Int, [Bind], Exp)

> type Id = String

> type Bind = (Id, Exp)

Expressions are defined as follows.

> data Exp =
>     Apply [Exp]
>   | Fun Id
>   | Prim Id
>   | Var Id
>   | Arg Shared Int
>   | Int Int

Function arguments are referred to by position using the 'Arg'
constructor; we assume they are already marked as unshared or shared.
One reason for this is that Jansen's encoding may hide the true
linearity of variables in case alternatives, so if Jansen's encoding
is used, the sharing analysis, albeit simple, needs to be done prior
to the removal of case expressions.  In this memo, we do not wish to
consider how best to implement case expressions, but see Memo 13 for
some thoughts on this.

Variables refer only to local let-bindings in a function's definition.

The compiler turns a program into a node which, when evaluated,
represents the result of the program.

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
>   where n = head [dash True n | (w, n) <- binds, v == w]
> compExp p binds code (Arg shared i) = (code, ARG shared i)
> compExp p binds code (Prim f) = (code, PRI f)
> compExp p binds code (Fun f) = (code, n)
>   where n = head [FUN arity is | (g, arity, is) <- p, f == g]
> compExp p binds code (Apply es) = compApp p binds False code es

> compApp p binds spine code es =
>   (code' ++ [if spine then PUSH ns else MKAP ns], APP False (length code'))
>   where (code', ns) = mapAccumL (compExp p binds) code es

Running a program
-----------------

The result of a program is defined by the following function.

> run :: Prog -> Int
> run p = eval [compile p] [] []

Performance measurements: updates avoided
-----------------------------------------

Here are the numbers of updates performed, applications unwound,
instructions executed, swaps and primitives reductions accross a range
of benchmark programs.

  PROGRAM   UPDATES   UNWINDS    INSTRS    SWAPS    PRIMS
  Fib       16718       33471     75316    25092    20913
  Sudoku    16839       39274     94298     8138     5391
  OrdList   40400      227358    611949        0        1
  Queens    55220      169355     98500    76546    43435
  Queens2   70339      146681    374976    17122     9044
  PermSort  65515      139635    364867    17325     8674
  MSS       96805      187826    266116    83388    41726
  Clausify  77848      243064    769995    24964    14317
  While     68815      160735    447688    31451    15726

To see the impact of the dynamic sharing analysis, look at the
differences between the numbers of updates and unwinds.  Without any
sharing analysis, these numbers would be the same.  On average, the
dynamic sharing analysis avoids 60% of updates.

Performance estimate
--------------------

How well would such a machine perform in hardware?  We make the
following estimate and compare it with the Reduceron from Matthew's
PhD.

  PROGRAM        PHD REDUCERON    SPINELESS REDUCERON
  Fib                   230135                 129666
  Sudoku                435996                  97397
  OrdList              1614678                 455078
  Queens                962788                 443056
  Queens2              1100675                 373775
  PermSort              932722                 353090
  MSS                  1485660                 468804
  Clausify             1662671                 639097
  While                1019827                 453986

What assumptions do we make about the hardware implementation of the
spineless Reduceron?  Well, many of these have been discussed in
previous memos, but here is a recap:

  1. Unwinding takes a single cycle, due to of memory-aligned
     applications.  Indeed, in the spineless experiments, all applications
     have been transformed so they have a maximum length of 4.

  2. Executing an instruction takes a single cycle, but two
     instructions in the same instruction sequence can be executed in
     parallel, thanks to the dual-port heap.  Again, all PUSH and MKAP
     instructions deal with applications of maximum length 4.

  3. A swap and a primitive reduction both take one cycle each.

  4. Updating takes '1 + n `div` 4' cycles where 'n' is the length of
     the normal-form.

Future developments
-------------------

Before attempting a hardware implementation of this machine, there are
three main improvements we'd like to explore: handling of case
expressions, speculative evaluation of primitives, and garbage
collection.
