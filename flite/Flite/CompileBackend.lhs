TODO:

- All non-constructors can replace the function index with a direct
  code pointer for much faster dispatch.  Constructors still need to
  be indices due to how case-of is implemented (currently).  Changing
  the constructors would require significant changes to the model, but
  would move Flite-C closer to the STG.

- Speaking of STG, the evaluator has some inefficiencies as it has to
  examine every object with no knowledge of the object.  STG embeds a
  pointer to a specialized evaluator into every AP.  This way we save
  the tag checking, the arity lookup, we can unroll the unwinding loop
  (as the size is known).  The update code should also be possible to
  integrate.


========================
REDUCERON MEMO 22'
Compiling F-lite to C
Matthew N, 30 April 2009
Tommy T, 2014-12-12
Tommy T, 2017-08-27
========================

This memo defines a compiler from supercombinators to portable C.  It
is intended as a C back-end to the F-lite implementation.

> module Flite.CompileBackend where

> import Data.List

Heap layout
-----------

A node is a tagged pointer, storable in a single word of memory.

> nodeType = "typedef uintptr_t Node;"


The least-significant bit of a node is a tag stating whether the node
is an INT or non-INT (AP or FUN).

  typedef enum {INT = 0, NON_INT} IntTag;

If a node is an INT, its remaining 31-bits is an unboxed integer.

The 2nd least-significant bit of a node distinguishes between AP and FUN, however
it's more efficient to consider both bits together:

  typedef enum {AP = 1, FUN = 3, } Tag;

If a node is an AP, its remaining 30 bits is a word-aligned heap
address.  Heap objects have their payload length in the first word, as
a tagged int.

If a node is a FUN, its remaining 30-bits contains a 6-bit arity and a
23-bit function identifier.

> destType = "typedef enum dest Dest;"

> macros = unlines
>   [ ""
>   , "static const Node TAGMASK    = 3;"
>   , ""
>   , "static const Node APTAG      = 1;"
>   , "static const Node FUNTAG     = 3;"

In order to get faster dispatch, we pre-scale the index by the index
by the table stride and create a complicated macro gotoFUN to avoid
the scaling twice.

>   , "const Node FUNPOS     =12;"
>   , ""
>   , "static bool isAP(Node n)             {return (n & TAGMASK) == APTAG; }"
>   , "static Node *getAP(Node n)           {return (Node*)(n - APTAG);}"
>   , "static Node makeAP(Node *p)          {return (Node)p + APTAG;}"
>   , ""

>   , "static bool isINT(Node n)            {return (n & 1) == 0;}"
>   , "static long getINT(Node n)           {return (long)n >> 1;}"
>   , "static Node makeINT(long i)          {return i << 1;}"

>   , "static bool isFUN(Node n)            {return (n & TAGMASK) == FUNTAG;}"
>   , "static Node getARITY(Node n)         {return (n >> 2) & 63;}"
>   , "static Node getFUN(Node n)           {return n >> FUNPOS;}"
>   , "static const long LOGW = sizeof(uintptr_t) == 8 ? 3 : 2;"
>   , "#define gotoFUN(n,o) goto **(void **)((void *)funEntry + ((n) >> (FUNPOS-LOGW)) + ((o) << LOGW))"
>   , "static Node makeFUN(long arity, long fun)"
>   , "                                     {return (fun << FUNPOS) + (arity << 2) + FUNTAG;}"

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
>   [ "register Node top;"           {- top of stack -}
>   , "register Node *restrict sp;"  {- stack pointer -}
>   , "register Node *hp;"           {- heap pointer -}
>   , "Update *usp;"                 {- update-stack pointer -}
>   ]

Swapping
--------

The following code swaps the top and 3rd elements of the stack and
flips the SWAPPED bit of the middle primitive.  It is used in the
evaluation of strict primitive functions.

> swapCode = unlines
>   [ "{"
>   , "  Node tmp;"
>   , "  tmp = top;"
>   , "  top = sp[-2];"
>   , "  sp[-2] = tmp;"
>   , "  sp[-1] ^= 1 << FUNPOS;"
>   , "}"
>   ]

Unwinding
---------

Unwinding copies an application from the heap onto the stack, and
pushes an update record onto the update stack.

> unwindCode = unlines
>   [ "{"
>   , "  assert(isAP(top));"
>   , "  Node *p = getAP(top);"
>   , "  usp++; usp->s = sp; usp->h = p;"
>   , "  assert(isINT(*p));"
>   , "  long n = getINT(*p++);"
>   , "  for (;;) {"
>   , "    top = *p++;"
>   , "    --n;"
>   , "    if (n == 0)"
>   , "        break;"
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
>   , "  long ari = arity(top);"
>   , "  assert(sp - ari >= stack);"
>   , "  for (;;) {"
>   , "    long args = sp - usp->s;"
>   , "    if (ari <= args)"
>   , "      break;"
>   , "    assert(usp != ustack);"
>   , "    Node *base = hp;"
>   , "    Node *p = usp->s;"
>   , "    *hp++ = makeINT(args + 1);"
>   , "    while (p < sp) *hp++ = *p++;"
>   , "    *hp++ = top;"

Install a forwarding pointer to the updated value

>   , "    assert(isINT(usp->h[0]));"
>   , "    long origSize = getINT(usp->h[0]);"
>   , "    usp->h[0] = makeINT(1);"
>   , "    usp->h[1] = makeAP(base);"

XXX This is a little annoying, but I want to keep objects complete.
However now we can potentially have a zero-sized object.

>   , "    if (origSize > 1)"
>   , "       usp->h[2] = makeINT(origSize - 2);"
>   , "    usp--;"
>   , "  }"
>   , "}"
>   ]

Evaluation driver
-----------------

Evalution proceeds depending on the element on top of the stack.

> evalCode = unlines
>   [ "EVAL:"
>   , "while (isAP(top))"
>   ,    unwindCode
>   , "EVAL_NO_AP:"
>   , "if (isFUN(top)) {"
>   , "    EVAL_FUN:"
>   , "    if (hp > heapFull) collect();"
>   ,      updateCode
>   , "    assert(isFUN(top));"
>   , "    gotoFUN(top, 0);"
>   , "}"
>   ,  updateCode

Invoke primitives.  At this point we know that top isn't an AP nor a
FUN, so it must be an INT.

>   , "if (isINT(sp[-2])) {"
>   , "    assert(isFUN(sp[-1]));"
>   , "    gotoFUN(sp[-1], 0);"
>   , "}"

>   ,  swapCode
>   , "goto EVAL;"
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

Each function body is implemented as a small block of code preceeded
by a label.  The label is stored in a table 'funEntry'.  To jump to
the code for a function, 'goto *funEntry[X]', where 'X' is the
function's identifier.

> switchCode (cs, ds) = unlines
>   [ "{"
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

where i is the index of the constructor, n is the arity of the
constructor, and (f+i) represents the function occuring in definitions
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
>   [ defLabel f
>   , "{"
>   , "  assert(isFUN(sp[-" ++ show (n+1) ++ "]));"
>   , "  gotoFUN(sp[-" ++ show (n+1) ++ "], " ++ show i ++ ");"
>   , "}"
>   , ""
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
> fun "(+)"      = "PRIM_PLUS"
> fun "(-)"      = "PRIM_MINUS"
> fun "(<=)"     = "PRIM_LEQ"
> fun "(==)"     = "PRIM_EQ"
> fun "(/=)"     = "PRIM_NEQ"
> fun "(.&.)"    = "PRIM_BINAND"
> fun "st32"     = "PRIM_ST32"
> fun "ld32"     = "PRIM_LD32"
> fun "emit"     = "PRIM_EMIT"
> fun "emitInt"  = "PRIM_EMITINT"
> fun " exit "   = "PRIM_EXIT"
> fun "_|_"      = "PRIM_UNDEFINED"
> fun ('s':'w':'a':'p':':':f) = fun f ++ "_SWAPPED"
> fun f          = "FUN_" ++ f

We also want a label variant of this

> label f       = "L_" ++ fun f

A shorthand for a common case

> defLabel f    = label f ++ ":"

> declareArgs :: Int -> String
> declareArgs n = unlines $ map save [1..n]
>   where save i = "Node " ++ arg i ++ " = sp[-" ++ show i ++ "];"

> declareLocals :: String
> declareLocals = "Node *base = hp;"

> type Locs = [(Id, Int)]

> node :: String -> Locs -> Node -> String
> node r vs (INT i)   = r ++ " = makeINT(" ++ show i ++ ");"
> node r vs (ARG i)   = r ++ " = " ++ arg (i+1) ++ ";"
> node r vs (FUN n f) = r ++ " = makeFUN(" ++ show n ++ "," ++ fun f ++ ");"
> node r vs (VAR v)   = r ++ " = makeAP(base+" ++ offset ++ ");"
>   where offset = show $ lookupVar v vs

> lookupVar v vs = case lookup v vs of { Nothing -> error msg ; Just i -> i }
>   where msg = error ("Unknown identifier '" ++ v ++ "'")

> app :: Locs -> App -> String
> app vs app = unlines $ header : map (node "*hp++" vs) app
>   where header = "*hp++ = makeINT (" ++ show (length app) ++ ");"

> spine :: Locs -> App -> String
> spine vs ns = unlines
>   [ unlines $ map (node "*sp++" vs) (init ns)
>   , node "top" vs (last ns)
>   ]

> varLocs :: Body -> Locs
> varLocs body = zip vs (scanl (+) 0 $ map length appsWithHeader)
>   where (vs, apps) = unzip body
>         appsWithHeader = map (undefined:) apps

> body :: App -> Body -> String
> body s b = unlines
>   [ concatMap (app vs . snd) b
>   , spine vs s
>   , "goto " ++ eval ++ ";"
>   ] where vs = varLocs b
>           eval = case last s of
>                  FUN _ _ -> "EVAL_FUN"
>                  _       -> "EVAL"

> defn :: Defn -> String
> defn (f, n, bs) = unlines
>   [ defLabel f
>   , "{"
>   , declareArgs n
>   , declareLocals
>   , "sp -= " ++ show n ++ ";"
>   , body (snd s) b
>   , "}"
>   , ""
>   ]
>   where s:b = [(v, reverse a) | (v, a) <- bs]

> defns :: [Defn] -> String
> defns = concatMap defn

Primitives
----------

Note: the order of primitives is such that the original primitive is
even which the swapped version is odd.  This makes is trivial to
transform one into the other.

> primIds :: [Id]
> primIds = concatMap (\p -> [p,  "swap:" ++ p]) l ++ ["_|_"]
>   where l = [ "(+)" , "(-)" , "(<=)" , "(==)", "(/=)",
>               "(.&.)", "st32", "ld32", "emit", "emitInt",
>               " exit " ]

> arithPrim :: Id -> String -> String
> arithPrim p op = unlines
>   [ defLabel p
>   , "{"
>   , "assert(isINT("++a++"));"
>   , "assert(isINT("++b++"));"
>   , "top = "++a++" " ++ op ++ " "++b++";"
>   , "sp -= 2;"
>   , "goto EVAL_NO_AP;"
>   , "}"
>   , ""
>   ]
>   where (a,b) = case p of
>                 's':_ -> ("sp[-2]","top")
>                 _     -> ("top","sp[-2]")

Ditto for boolean operator.

> boolPrim :: Id -> String -> String
> boolPrim p op = unlines
>   [ defLabel p
>   , "{"
>   , "assert(isINT("++a++"));"
>   , "assert(isINT("++b++"));"
>   , "top = (long)"++a++" " ++ op ++ " (long)"++b++" ? "
>       ++ "makeFUN(1," ++ fun "True"  ++ ") "
>       ++ ": makeFUN(1," ++ fun "False" ++ ");"
>   , "sp -= 2;"
>   , "goto EVAL_FUN;"
>   , "}"
>   , ""
>   ]
>   where (a,b) = case p of
>                 's':_ -> ("sp[-2]","top")
>                 _     -> ("top","sp[-2]")

Print the top stack element.

> emitPrim :: Id -> String -> String -> String
> emitPrim p format cast = unlines
>   [ defLabel p
>   , "{"
>   , "assert(isINT("++a++"));"
>   , "printf(\"" ++ format ++ "\", " ++ cast ++ "getINT(" ++ a ++ "));"
>   , "top = " ++ b ++ ";"
>   , "sp -= 2;"
>   , "goto EVAL;"
>   , "}"
>   , ""
>   ]
>   where (a,b) = case p of
>                 's':_ -> ("sp[-2]","top")
>                 _     -> ("top","sp[-2]")

Sential Exit primitive, to save us from special-cases the empty stack
case.

> exitPrim :: Id -> String
> exitPrim p = unlines
>   [ defLabel p
>   , "{"
>   , case p of
>       's':_ -> "assert(0);" -- execution should never get here
>       _     -> "assert(sp[-2] == makeINT(42));"
>   , "sp -= 2;"
>   , "goto EXIT;"
>   , "}"
>   , ""
>   ]


> st32Prim :: Id -> String
> st32Prim p = unlines
>   [ defLabel p
>   , "{"
>   , "assert(isINT("++a++"));"
>   , "assert(isINT("++b++"));"
>   , "int addr = getINT("++a++");"
>   , "int value = getINT("++b++");"
>   , "if (addr == 0) putchar(value);"
>   , "top = sp[-3];"
>   , "sp -= 3;"
>   , "goto EVAL;"
>   , "}"
>   , ""
>   ]
>   where (a,b) = case p of
>                 's':'w':_ -> ("sp[-2]","top")
>                 _     -> ("top","sp[-2]")

> ld32Prim :: Id -> String
> ld32Prim p = unlines
>   [ defLabel p
>   , "{"
>   , "assert(isINT("++a++"));"
>   , "int addr = getINT("++a++");"
>   , "top = makeINT(getchar());"
>   , "sp -= 2;"
>   , "goto EVAL_NO_AP;"
>   , "}"
>   , ""
>   ]
>   where (a,b) = case p of
>                 's':_ -> ("sp[-2]","top")
>                 _     -> ("top","sp[-2]")

> undefPrim :: String
> undefPrim = unlines
>   [ defLabel "_|_"
>   , "{"
>   , "printf(\"ERROR: bottom!\\n\");"
>   , "goto EXIT;"
>   , "}"
>   , ""
>   ]

> prims :: String
> prims = unlines
>   [ arithPrim "(+)" "+"
>   , arithPrim "swap:(+)" "+"
>   , arithPrim "(-)" "-"
>   , arithPrim "swap:(-)" "-"
>   , arithPrim "(.&.)" "&"
>   , arithPrim "swap:(.&.)" "&"
>   , boolPrim "(<=)" "<="
>   , boolPrim "swap:(<=)" "<="
>   , boolPrim "(==)" "=="
>   , boolPrim "swap:(==)" "=="
>   , boolPrim "(/=)" "!="
>   , boolPrim "swap:(/=)" "!="
>   , emitPrim "emit" "%c" "(char)"
>   , emitPrim "swap:emit" "%c" "(char)"
>   , emitPrim "emitInt" "%ld" ""
>   , emitPrim "swap:emitInt" "%ld" ""
>   , exitPrim " exit "
>   , exitPrim "swap: exit "
>   , st32Prim "st32"
>   , st32Prim "swap:st32"
>   , ld32Prim "ld32"
>   , ld32Prim "swap:ld32"
>   , undefPrim
>   ]

Garbage collection
------------------

> copyAPCode = unlines
>   [ "Node copyAP(Node src) {"

If the AP has been copied already, a forwarding pointer pointing to
toSpace would have been left in the 1st word.

>   , "  Node *from = getAP(src);"
>   , "  assert(isINT(from[0]));"
>   , "  if (isAP(from[1]) && toSpace <= getAP(from[1]) && getAP(from[1]) < toSpaceEnd)"
>   , "      return from[1];"

Else, copy the AP to toSpace and leave a forwarding pointer

>   , "  Node new = makeAP(tsp);"
>   , "  for (long n = getINT(from[0]); n >= 0; --n)"
>   , "    *tsp++ = *from++;"
>   , "  return getAP(src)[1] = new;"
>   , "}"
>   ]

> copyCode = unlines
>   [ "void copy(void) {"
>   , "  for (Node *low = toSpace; low < tsp; ++low)"
>   , "    if (isAP(*low))"
>   , "      *low = copyAP(*low);"
>   , "}"
>   ]

> collectCode = unlines
>   [ "void collect (void) {"
>   , "  tsp = toSpace;"
>   , "  for (Node *p1 = stack; p1 < sp; ++p1)"
>   , "    if (isAP(*p1))"
>   , "       *p1 = copyAP(*p1);"
>   , "  if (isAP(top))"
>   , "       top = copyAP(top);"
>   , "  copy();"
>   , ""
>   , "  Update *p3 = ustack;"
>   , "  for (Update *p2 = ustack+1; p2 <= usp; ++p2) {"
>   , "    Node n = *p2->h;"
>   , "    if (isAP(n) && toSpace <= getAP(n) && getAP(n) <= toSpaceEnd) {"
>   , "      p3++;"
>   , "      p3->s = p2->s;"
>   , "      p3->h = getAP(n);"
>   , "    }"
>   , "  }"
>   , ""
>   , "  usp = p3;"
>   , "  hp = tsp;"
>   , "  Node *p1 = toSpace; toSpace = heap; heap = p1;"
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
>   , "Node *tsp;"          {- to-space pointer -}
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

Push the sentinel exit on the stack

> pushSentinelExit :: String
> pushSentinelExit = unlines
>   [ "*sp++ = makeINT(42);"
>   , "*sp++ = makeFUN(2," ++ fun " exit " ++ ");"
>   ]

Push sentinel update record which will never satisfy the "ari > args"
check.

> pushSentinelUpdate :: String
> pushSentinelUpdate = unlines
>   [ "usp->s = stack - 99999;"
>   , "usp->h = 0;"
>   ]

Program compilation
-------------------

> funIds :: Program -> [String]
> funIds (cs, ds) = map first cs ++ map first ds
>   where first (x, y, z) = x

Note: primitives comes first so we are guaranteed the first is even.

> declareFuns :: Program -> String
> declareFuns p =
>   unlines $ ["enum dest {"] ++ [fun f ++ ","| f <- primIds ++ funIds p] ++ ["};"]

> program :: Program -> String
> program p = unlines
>   [ "#include <stdio.h>"
>   , "#include <stdlib.h>"
>   , "#include <stdint.h>"
>   , "#include <stdbool.h>"
>   , "#include <string.h>"
>   , "#include <assert.h>"
>   , nodeType
>   , destType
>   , updateType
>   , macros
>   , declareFuns p
>   , globals
>   , "int main(int argc, char *argv[]) {"
>   , "  static const void *funEntry[] = {"
>   , unlines $ ["   &&" ++ label f ++ ","| f <- primIds ++ funIds p]
>   , "};"
>   , registers
>   , copyAPCode
>   , copyCode
>   , collectCode
>   , allocate 8000000 1000000
>   , pushSentinelExit
>   , pushSentinelUpdate
>   , "goto " ++ label "main" ++ ";"
>   , switchCode p
>   , evalCode
>   , "EXIT:"
>   , "#ifdef PRINT_RESULT"
>   , "assert(isINT(top));"
>   , "printf(\"%ld\\n\", getINT(top));"
>   , "#endif"
>   , "return 0;"
>   , "}"
>   ]
