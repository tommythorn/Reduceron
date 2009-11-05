========================
REDUCERON MEMO 22
Compiling F-lite to C
Matthew N, 30 April 2009
========================

This memo defines a compiler from supercombinators to portable C.  It
is intended as a back-end to the F-lite implementation.  The aim is to
run F-lite programs on an FPGA soft-core, such as the Microblaze.

> module Flite.CompileBackend where

> import Data.List

Heap layout
-----------

A node is a tagged pointer, storable in a single word of memory.

> nodeType = "typedef unsigned long Node;"

The least-significant bit of a node is a tag stating whether the node
is an AP, containing a pointer to an application (a sequence of nodes)
on the heap, or an OTHER, containing something else.

  typedef enum {AP = 0, OTHER = 1} Tag;

The 2nd least-significant bit of a node is a flag stating whether or
not the node is the final node of an application.

> macros = unlines
>   [ "#define isFinal(n) ((n) & 2)"
>   , "#define clearFinal(n) ((n) & (~2))"
>   , "#define setFinal(n) ((n) | 2)"
>   , "#define markFinal(n,final) ((final) ? setFinal(n) : clearFinal(n))"

If a node is an AP, its remaining 30 bits is a word-aligned heap
address.

>   , "#define getAP(n) ((Node *) ((n) & (~3)))"

If the node is an OTHER, its 3rd least-significant bit contains a
sub-tag stating whether the the node is an INT or a FUN.

  typedef enum {INT = 0, FUN = 1} Subtag;

If a node is an INT, its remaining 29-bits is an unboxed integer.

>   , "#define getINT(n) (((signed long) n) >> 3)"

If a node is a FUN, its remaining 29-bits contains a 6-bit arity and a
23-bit function identifier.

>   , "#define getARITY(n) (((n) >> 3) & 63)"
>   , "#define getFUN(n) ((n) >> 9)"

More precisely:

>   , "#define isAP(n) (((n) & 1) == 0)"
>   , "#define isINT(n) (((n) & 5) == 1)"
>   , "#define isFUN(n) (((n) & 5) == 5)"
>   , "#define makeAP(a,final) ((unsigned long) (a) | ((final) << 1))"
>   , "#define makeINT(i,final) (((i) << 3) | ((final) << 1) | 1)"
>   , "#define makeFUN(arity,f,final) " ++
>              "(((f) << 9) | ((arity) << 3) | ((final) << 1) | 5)"
>   , "#define arity(n) (isFUN(n) ? getARITY(n) : 1)"
>   ]

Update records
--------------

An update record is a pair containing a stack pointer (to detect when
a head normal form has been reached) and a heap pointer (stating where
to write the head normal form).

> updateType = "typedef struct { Node *s; Node *h; } Update;"

Registers
---------

> registers = unlines
>   [ "Node top;"           {- top of stack -}
>   , "Node *sp;"           {- stack pointer -}
>   , "Node *hp;"           {- heap pointer -}
>   , "Node *tsp;"          {- to-space pointer -}
>   , "Update *usp;"        {- update-stack pointer -}
>   , "unsigned int dest;"  {- destination address for computed jumps -}
>   ]

Swapping
--------

The following code swaps the top two elements of the stack.  It is
used in the evaluation of strict primitive functions.

> swapCode = unlines
>   [ "{"
>   , "  Node tmp;"
>   , "  tmp = top;"
>   , "  top = sp[-1];"
>   , "  sp[-1] = tmp;"
>   , "}"
>   ]

Unwinding
---------

Unwinding copies an application from the heap onto the stack, and
pushes an update record onto the update stack.

> unwindCode = unlines
>   [ "{"
>   , "  Node *p;"
>   , "  p = getAP(top);"
>   , "  usp++; usp->s = sp; usp->h = p;"
>   , "  for (;;) {"
>   , "    top = *p++;"
>   , "    if (isFinal(top)) break;"
>   , "    *sp++ = top;"
>   , "  }"
>   , "}"
>   ]

Updating
--------

The following code determines if a normal form has been reached, and
if so, performs an update.

> updateCode = unlines
>   [ "{"
>   , "  unsigned int args, ari;"
>   , "  Node *base;"
>   , "  Node *p;"
>   , "  ari = arity(top);"
>   , "  if (sp - ari < stack) goto EXIT;"
>   , "  DO_UPDATE:"
>   , "  args = ((unsigned int) (sp - usp->s));"
>   , "  if (ari > args && usp > ustack) {"
>   , "    base = hp;"
>   , "    p = sp - args;"
>   , "    while (p < sp) *hp++ = clearFinal(*p++);"
>   , "    *hp++ = setFinal(top);"
>   , "    *(usp->h) = makeAP(base, 1);"
>   , "    usp--;"
>   , "    goto DO_UPDATE;"
>   , "  }"
>   , "}"
>   ]

Evaluation driver
-----------------

Evalution proceeds depedning on the element on top of the stack.

> evalCode = unlines
>   [ "EVAL:"
>   , "if (isAP(top)) {"
>   ,    unwindCode
>   , "  goto EVAL;"
>   , "}"
>   , "else {"
>   , "  EVAL_OTHER:"
>   , "  if (hp > heapFull) collect();"
>   ,    updateCode
>   , "  if (isFUN(top)) {"
>   , "    dest = getFUN(top);"
>   , "    goto CALL;"
>   , "  }"
>   , "  else {"
>   ,      swapCode
>   , "    goto EVAL;"
>   , "  }"
>   , "}"
>   ]

Abstract syntax of source code
------------------------------

The body of a function is a list of identifier/application pairs.  The
first element in the list contains the spine application of the
function.

> type Binding = (Id, App)

> type Body = [Binding]

An application is a list of nodes.

> type App = [Node]

> data Node
>   = VAR Id        {- variable reference -}
>   | ARG Int       {- argument reference -}
>   | FUN Arity Id  {- function identifier -}
>   | INT Int       {- integer -}
>   deriving Show

> type Id = String

A function definition consists of an identifier, an arity, and a body.

> type Defn = (Id, Arity, Body)

> type Arity = Int

For example, the F-lite function definition

   s f g x = f x (g x);

is represented in abstract syntax as follows.

  ("s", 3, [ ("v0", [ARG 0, ARG 2, VAR "v1"])
           , ("v1", [ARG 1, ARG 2])
           ])

A data constructor consists of identifier, an arity, and an index.

> type Cons = (Id, Arity, Index)

> type Index = Int

A program consists of a list of constuctors and a list of function
definitions.

> type Program = ([Cons], [Defn])

Function calling
----------------

Each function body is implemented as a case alternative in a large
switch statement.  To jump to the code for a function, place the
function's identifier in the 'dest' register and then 'goto CALL'.
This double jump is not very efficient, but its not obvious how to do
any better in C.

> switchCode (cs, ds) = unlines
>   [ "CALL:"
>   , "switch (dest)"
>   , "{"
>   , prims          -- primitive definitions
>   , constrs cs     -- constructor definitions
>   , defns ds       -- function definitions
>   , "}"
>   ]

Constructor compilation
-----------------------

Each constructor C used in the program is treated as a function with
the following definition.

  Ci v1 ... vn f = (f+i) v1 ... vn f

where i is the index of the constructor, n is the artiy of the
constructor, and (f+i) represents the function occuring i definitions
after the definition of f in the program code.  It is assumed that
case alternatives occur contiguously, ordered by index.  For example,
the F-lite program

  rev acc Nil = acc;
  rev acc (Cons x xs) = rev (Cons x acc) xs;

is transformed down to

  rev acc xs = xs revCons acc;
  revCons x xs acc = rev (Cons x acc) xs;
  revNil acc = acc;

if Cons has index 0 and Nil has index 1.

(See Memo 13 for a more detailed explanation of how constructors and
case expressions are treated.)

> cons :: Cons -> String
> cons (f, n, i) = unlines
>   [ "case " ++ fun f ++ ":"
>   , "{"
>   , "dest = getFUN(sp[-" ++ show (n+1) ++ "]) + " ++ show i ++ ";"
>   , "goto CALL;"
>   , "}"
>   , "break;"
>   ]

NB. No update is required because a case expression is not a normal form.

> constrs :: [Cons] -> String
> constrs = concatMap cons

Function compilation
--------------------

> arg :: Int -> String
> arg i = "ARG_" ++ show i

> var :: Id -> String
> var v = "VAR_" ++ v

Map F-lite primitives to suitable C identifiers.

> fun :: Id -> String
> fun "(+)" = "PRIM_PLUS"
> fun "(-)" = "PRIM_MINUS"
> fun "(<=)" = "PRIM_LEQ"
> fun "(==)" = "PRIM_EQ"
> fun "(/=)" = "PRIM_NEQ"
> fun "emit" = "PRIM_EMIT"
> fun "emitInt" = "PRIM_EMITINT"
> fun "_|_" = "PRIM_UNDEFINED"
> fun f = "FUN_" ++ f

> declareArgs :: Int -> String
> declareArgs n = unlines $ map save [1..n]
>   where save i = "Node " ++ arg i ++ " = sp[-" ++ show i ++ "];"

> declareLocals :: String
> declareLocals = "Node *base = hp;"

> type Locs = [(Id, Int)]

> node :: String -> Locs -> String -> Node -> String
> node r vs final (INT i) =
>   r ++ " = makeINT(" ++ show i ++ "," ++ final ++ ");"
> node r vs final (ARG i) =
>   r ++ " = markFinal(" ++ arg (i+1) ++ "," ++ final ++ ");"
> node r vs final (VAR v) =
>   r ++ " = makeAP(base+" ++ offset ++ "," ++ final ++ ");"
>   where offset = show $ lookupVar v vs
> node r vs final (FUN n f) = 
>   r ++ " = makeFUN(" ++ show n ++ "," ++ fun f ++ "," ++ final ++ ");"

> lookupVar v vs = case lookup v vs of { Nothing -> error msg ; Just i -> i }
>   where msg = error ("Unknown identifier '" ++ v ++ "'")

> app :: Locs -> App -> String
> app vs app = unlines $ zipWith (node "*hp++" vs) finals app
>   where finals = map (const "0") (init app) ++ ["1"]

> spine :: Locs -> App -> String
> spine vs ns = unlines
>   [ unlines $ map (node "*sp++" vs "0") (init ns)
>   , node "top" vs "0" (last ns)
>   ]

> varLocs :: Body -> Locs
> varLocs body = zip vs (scanl (+) 0 (map length apps))
>   where (vs, apps) = unzip body

> body :: App -> Body -> String
> body s b = unlines
>   [ concatMap (app vs . snd) b
>   , spine vs s
>   , "goto EVAL;"
>   ] where vs = varLocs b

> defn :: Defn -> String
> defn (f, n, bs) = unlines
>   [ "case " ++ fun f ++ ":"
>   , "{"
>   , declareArgs n
>   , declareLocals
>   , "sp -= " ++ show n ++ ";"
>   , body (snd s) b
>   , "}"
>   , "break;"
>   ]
>   where s:b = [(v, reverse a) | (v, a) <- bs]

> defns :: [Defn] -> String
> defns = concatMap defn

Primitives
----------

> primIds :: [Id]
> primIds =
>   [ "(+)" , "(-)" , "(<=)" , "(==)", "(/=)", "emit", "emitInt", "_|_" ]

Apply primitive arithmetic operator to 2nd and 3rd stack elements;
store result in top.

> arithPrim :: Id -> String -> String
> arithPrim p op = unlines
>   [ "case " ++ fun p ++ ":"
>   , "{"
>   , "top = makeINT(getINT(sp[-1]) " ++ op ++ " getINT(sp[-2]),0);"
>   , "sp -= 2;"
>   , "goto EVAL;"
>   , "}"
>   , "break;"
>   ]

Ditto for boolean operator.

> boolPrim :: Id -> String -> String
> boolPrim p op = unlines
>   [ "case " ++ fun p ++ ":"
>   , "{"
>   , "top = (getINT(sp[-1]) " ++ op ++ " getINT(sp[-2])) ? "
>       ++ "makeFUN(1," ++ fun "True"  ++ ",0) "
>       ++ ": makeFUN(1," ++ fun "False" ++ ",0);"
>   , "sp -= 2;"
>   , "goto EVAL;"
>   , "}"
>   , "break;"
>   ]

Print the second stack element.

> emitPrim :: Id -> String -> String
> emitPrim p format = unlines
>   [ "case " ++ fun p ++ ":"
>   , "{"
>   , "top = sp[-2];"
>   , "printf(\"" ++ format ++ "\", getINT(sp[-1]));"
>   , "sp -= 2;"
>   , "goto EVAL;"
>   , "}"
>   , "break;"
>   ]

> undefPrim :: String
> undefPrim = unlines
>   [ "case " ++ fun "_|_" ++ ":"
>   , "{"
>   , "printf(\"ERROR: bottom!\\n\");"
>   , "goto EXIT;"
>   , "}"
>   , "break;"
>   ]

> prims :: String
> prims = unlines
>   [ arithPrim "(+)" "+"
>   , arithPrim "(-)" "-"
>   , boolPrim "(<=)" "<="
>   , boolPrim "(==)" "=="
>   , boolPrim "(/=)" "!="
>   , emitPrim "emit" "%c"
>   , emitPrim "emitInt" "%i"
>   , undefPrim
>   ]

Garbage collection
------------------

> copyAPCode = unlines
>   [ "Node *copyAP(Node *src) {"
>   , "  Node n;"
>   , "  Node *from = src;"
>   , "  Node *dst = tsp;"
>   , "  n = *from;"
>   , "  if (isAP(n)) {"
>   , "    Node *loc = getAP(n);"
>   , "    if (loc >= toSpace && loc < toSpaceEnd) return loc;"
>   , "  }"
>   , "  do {"
>   , "    n = *from++; *tsp++ = n;"
>   , "  } while (! isFinal(n));"
>   , "  *src = (Node) dst;"
>   , "  return dst;"
>   , "}"
>   ]

> copyCode = unlines
>   [ "void copy() {"
>   , "  Node n;"
>   , "  Node *low = toSpace;"
>   , "  while (low < tsp) {"
>   , "    n = *low;"
>   , "    if (isAP(n)) {"
>   , "      Node *loc = copyAP(getAP(n));"
>   , "      *low = markFinal((Node) loc, isFinal(n));"
>   , "    }"
>   , "    low++;"
>   , "  }"
>   , "}"
>   ]

> collectCode = unlines
>   [ "void collect () {"
>   , "  Node n;"
>   , "  Node *p1;"
>   , "  Update *p2;"
>   , "  Update *p3;"
>   , "  tsp = toSpace;"
>   , "  p1 = stack;"
>   , "  while (p1 < sp) {"
>   , "    n = *p1;"
>   , "    if (isAP(n)) *p1 = (Node) copyAP(getAP(n));"
>   , "    p1++;"
>   , "  }"
>   , "  if (isAP(top)) top = (Node) copyAP(getAP(top));"
>   , "  copy();"
>   , "  p2 = ustack+1;"
>   , "  p3 = ustack;"
>   , "  while (p2 <= usp) {"
>   , "    n = *(p2->h);"
>   , "    if (isAP(n) && getAP(n) >= toSpace && getAP(n) <= toSpaceEnd) {"
>   , "      p3++;"
>   , "      p3->s = p2->s;"
>   , "      p3->h = getAP(n);"
>   , "    }"
>   , "    p2++;"
>   , "  }"
>   , "  usp = p3;"
>   , "  hp = tsp;"
>   , "  p1 = toSpace; toSpace = heap; heap = p1;"
>   , "  p1 = toSpaceEnd; toSpaceEnd = heapEnd; heapEnd = p1;"
>   , "  p1 = toSpaceFull; toSpaceFull = heapFull; heapFull = p1;"
>   , "}"
>   ]

Global variables
----------------

We need to store the beginning and end address of each memory
partition, to detect termination and exhaustion.

> globals :: String
> globals = unlines
>   [ "Node *heap;"
>   , "Node *heapEnd;"
>   , "Node *heapFull;"
>   , "Node *toSpace;"
>   , "Node *toSpaceEnd;"
>   , "Node *toSpaceFull;"
>   , "Node *stack;"
>   , "Node *stackEnd;"
>   , "Update *ustack;"
>   , "Update *ustackEnd;"
>   ]

Memory allocation
-----------------

> allocate :: Int -> Int -> String
> allocate heapSize stackSize = unlines
>   [ "heap = (Node *) malloc(sizeof(Node) * " ++ show heapSize ++ ");"
>   , "hp = heap;"
>   , "heapEnd = heap + " ++ show heapSize ++ ";"
>   , "heapFull = heapEnd - 1000;"
>   , "toSpace = (Node *) malloc(sizeof(Node) * " ++ show heapSize ++ ");"
>   , "tsp = toSpace;"
>   , "toSpaceEnd = toSpace + " ++ show heapSize ++ ";"
>   , "toSpaceFull = toSpaceEnd - 1000;"
>   , "stack = (Node *) malloc(sizeof(Node) * " ++ show stackSize ++ ");"
>   , "sp = stack;"
>   , "stackEnd = stack + " ++ show stackSize ++ ";"
>   , "ustack = (Update *) malloc(sizeof(Update) * " ++ show stackSize ++ ");"
>   , "usp = ustack;"
>   , "ustackEnd = ustack + " ++ show stackSize ++ ";"
>   ]

Program compilation
-------------------

> funIds :: Program -> [String]
> funIds (cs, ds) = map first cs ++ map first ds
>   where first (x, y, z) = x

> declareFuns :: Program -> String
> declareFuns p =
>   unlines $ [def f i | (f, i) <- zip (primIds ++ funIds p) [0..]]
>   where def f i = "#define " ++ fun f ++ " " ++ show i

> program :: Program -> String
> program p = unlines
>   [ "#include <stdio.h>"
>   , "#include <stdlib.h>"
>   , nodeType
>   , updateType
>   , macros
>   , declareFuns p
>   , registers
>   , globals
>   , copyAPCode
>   , copyCode
>   , collectCode
>   , "int main(void) {"
>   , allocate 8000000 1000000
>   , "dest = " ++ fun "main" ++ ";"
>   , switchCode p
>   , evalCode
>   , "EXIT:"
>   , "return 0;"
>   , "}"
>   ]
