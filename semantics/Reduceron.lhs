> module Reduceron where
> import Data.List

[[This is based on text extracted from

  reduceron-report.pdf
  The Reduceron Reconfigured
  Matthew Naylor Colin Runciman
  University of York, UK {mfn,colin}@cs.york.ac.uk

This defines the basic Reduceron machine.]]



2.7 Template Code

We are now very close to the template code that can be executed by the
Reduceron. We shall define template code as a Haskell data type, paving
the way for an executable semantics to be defined in the next
section. To highlight the semantics, each semantic definition is
prefixed with a '>' symbol.

In template code, a program is defined to be a list of templates.

> type Prog = [Template]

A template represents a function definition. It contains an arity, a
spine application and a list of nested applications.

> type Template = (Arity, App, [App])
> type Arity = Int

The spine application holds the let-body of a definition's expression
graph and the nested applications hold the let-bindings. Applications
are flat and are represented as a list of atoms.

> type App = [Atom]

An atom is a small, tagged piece of non-recursive data, defined in
Figure 2. The following paragraphs define how programs are translated
to template code.

Functions: Given a list of function definitions

   f0 x0 = e0 , . . . , fn xn = en

each function identifier fi occurring in e0 ... en is translated to an
atom FUN #f i where #f is the arity of function f.

Arguments: In each definition f x0 . . . xn = e, each variable xi
occurring in e is translated to an atom ARG i.

Let-Bound Variables: In each expression graph

  let { x0 = e0 ; . . . ; xn = en } in e

each xi occurring in e, e0 . . . en is translated to an atom
PTR i.

Integers, Primitives, and Constructors:  An integer literal n, a
primitive p, and a constructor Ci are translated to atoms INT n, PRI
p, and CON #Ci i respectively.

Case Tables: Given a list of function definitions

  f0 x0 = e0 , . . . , fn xn = en

each case table <fi , . . . fj > occurring in e0 . . . en is
translated to an atom TAB i. We assume that the functions in each case
table are defined contiguously in the program.

Example: The template code for the program

  main = tri 5
  tri n = let x = n (<=) in 1 x <falseCase, trueCase> n
  falseCase t n =
     let {x0 = tri x1 (+); x1 = 1 x2; x2 = n (-)} in n x0
  trueCase t n = 1

is as follows.

> tri5 :: Prog
> tri5 = [ (0, [FUN 1 1, INT 5], [])
>        , (1, [INT 1, PTR 0, TAB 2, ARG 0],
>              [[ARG 0, PRI "(<=)"]])
>        , (2, [ARG 1, PTR 0],
>              [[FUN 1 1, PTR 1, PRI "(+)"],
>               [INT 1, PTR 2],
>               [ARG 1, PRI "(-)"]])
>        , (2, [INT 1], []) ]



Figure 2. Syntax of atoms in template code.

> data Atom
>   = FUN Arity Int
>   | ARG Int
>   | PTR Int
>   | CON Arity Int
>   | INT Int
>   | PRI String
>   | TAB Int
>   deriving Show



3. Operational Semantics

This section defines a small-step operational semantics for the
Reduceron. There are two main reasons for presenting a semantics: (1)
to define precisely how the Reduceron works; and (2) to highlight the
low-level parallelism present in graph reduction that is exploited by
the Reduceron. We have found it very useful to encode the semantics
directly in Haskell. Before we commit to a low-level implementation,
we can assess the complexity and performance of different design
decisions and optimisations.

At the heart of the semantic definition is the small-step state
transition function

> step :: State -> State

where the state is a 4-tuple comprising a program, a heap, a reduction
stack, and an update stack.

> type State = (Prog, Heap, Stack, UStack)

The heap is modelled as a list of applications, and can be indexed by a heap-address.

> type Heap = [App]
> type HeapAddr = Int

An element on the heap can be modified using the update function.

> update :: HeapAddr -> App -> Heap -> Heap
> update i a as = take i as ++ [a] ++ drop (i+1) as

The reduction stack is also modelled as a list of nodes, with the top
stack element coming first and the bottom element coming last.

> type Stack = [Atom]
> type StackAddr = Int

There is also an update stack.

> type UStack = [Update]
> type Update = (StackAddr, HeapAddr)

The meaning of a program p is defined by run p where

> run :: Prog -> Int
> run p = eval initialState
>   where initialState = (p, [], [FUN 0 0], [])

> eval (p, h, [INT i], u) = i
> eval s = eval (step s)

The initial state of the evaluator comprises a program, an empty heap,
a singleton stack containing a call to main, and an empty update
stack. The main template has arity 0 and is assumed to be the template
at address 0. To illustrate, run tri5 yields 15. In the following
sections, the central step function is defined.


3.1 Primitive Reduction

The prim function applies a primitive function to two arguments
supplied as fully-evaluated integers.

> prim :: String -> Atom -> Atom -> Atom
> prim "(+)" (INT n) (INT m) = INT (n+m)
> prim "(-)" (INT n) (INT m) = INT (n-m)
> prim "(<=)" (INT n) (INT m) = bool (n<=m)

The comparison primitive returns a boolean value. Both boolean
constructors have arity 0; False has index 0 and True has index 1.

> bool :: Bool -> Atom
> bool False = CON 0 0
> bool True = CON 0 1


3.2 Normal Forms

The number of arguments demanded by an atom on top of the reduction
stack is defined by the arity function.

> arity :: Atom -> Arity
> arity (FUN n i) = n
> arity (INT i) = 1
> arity (CON n i) = n+1
> arity (PRI p) = 2

To reduce an integer, the evaluator demands one argument as shown in
rewrite rule (2). And to reduce a constructor of arity n, the
evaluator requires n + 1 arguments (the constructor's arguments and
the case table) as shown in rewrite rule (3).

The arity of an atom is only used to detect when a normal form is
reached. A normal form is an application of length n whose first atom
has arity >= n.

Some functions, such as case-alternative functions, are statically
known never to be partially-applied, so they cannot occur as the first
atom of a normal form. Such a function, say with address n, can be
represented by the atom FUN 0 n.


3.3 Step-by-Step Reduction

There is one reduction rule for each possible type of atom that can
appear on top of the reduction stack.

Unwinding: If the top of the reduction stack is a pointer x to an
application on the heap, evaluation proceeds by unwinding: copying the
application from the heap to the reduction stack where it can be
reduced. We must also ensure that when evaluation of the application
is complete, the location x on the heap can be updated with the
result. So we push onto the update stack the heap address x and the
current size of the reduction stack.

> step (p, h, PTR x:s, u) = (p, h, h!!x ++ s, upd:u)
>   where upd = (1+length s, x)

Updating: Evaluation of an application is known to be complete when an
argument is demanded whose index is larger than n, the difference
between the current size of the reduction stack and the stack address
of the top update. If this condition is met, then a normal form of
arity n is on top of the reduction stack and must be written to the
heap.

> step (p, h, top:s, (sa,ha):u)
>   | arity top > n = (p, h', top:s, u)
>   where
>     n = 1+length s - sa
>     h' = update ha (top:take n s) h

Integers and Primitives: Integer literals and primitive functions are
reduced as described in Section 2.3.

> step (p, h, INT n:x:s, u) = (p, h, x:INT n:s, u)
> step (p, h, PRI f:x:y:s, u) = (p, h, prim f x y:s, u)

Constructors: Constructors are reduced by indexing a case table, as
described in Section 2.4.

> step (p, h, CON n j:s, u) = (p, h, FUN 0 (i + j):s,u)
>   where TAB i = s !! n

There is insufficient information available to compute the arity of the
case-alternative function at address i+j. However, an arity of zero
can be used because a case-alternative function is statically known
not to be partially applied (Section 3.2).

Function Application: To apply a function f of arity n, n + 1 elements
are popped off the reduction stack, the spine application of the body
of f is instantiated and pushed onto the reduction stack, and the
remaining applications are instantiated and appended to the heap.

> step (p, h, FUN n f:s, u) = (p, h', s', u)
>   where
>     (pop, spine, apps) = p !! f
>     h' = h ++ map (instApp s h) apps
>     s' = instApp s h spine ++ drop pop s

Instantiating a function body involves replacing the formal parameters
with arguments from the reduction stack and turning relative pointers
into absolute ones.

> instApp :: Stack -> Heap -> App -> App
> instApp s h = map (inst s (length h))

> inst :: Stack -> HeapAddr -> Atom -> Atom
> inst s base (PTR p) = PTR (base + p)
> inst s base (ARG i) = s !! i
> inst s base a = a







[[Adding a bit of code to play with this]]

> runT :: Prog -> IO ()
> runT p = evalT 0 initialState
>   where initialState = (p, [], [FUN 0 0], [])

> evalT :: Int -> State -> IO ()
> evalT n (p, h, [INT i], u) =
>   putStrLn ("Evaluation Terminated with result "++show i)
>
> evalT n s = do
>   putStrLn (show n ++ ":")
>   showState s
>   evalT (n+1) (step s)

> showState :: State -> IO ()
> showState (p, h, s, u) = do
>   putStrLn ("Heap  :" ++ show h)
>   putStrLn ("Stack :" ++ stack)
>   putStrLn ("UStack:" ++ show u)
>   putStrLn ""
>   where
>   stack = concat (intersperse " " (map showAtom  s))
>   showAtom :: Atom -> String
>   showAtom a = case a of
>      FUN a 0 -> "Fmain"
>      FUN a i -> "F" ++ show i
>      ARG i   -> "a" ++ show i
>      PTR i   -> "#" ++ show i
>      CON  a i-> "CON " ++ show a ++ " " ++ show i
>      INT i   -> show i
>      PRI p   -> p
>      TAB i   -> "T" ++ show i

> main2 = putStrLn (show (run tri5))
> main = runT tri5
