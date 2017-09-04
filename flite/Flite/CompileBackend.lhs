TODO:

- The exit check code can be eliminated by added a special exit
  primitive and placing a sentinel call on the stack before evaluation
  starts.

- Similarily, it ought to be possible to eliminate the "|| usp <=
  ustack" part of the check in the updateCode by placing a sentinel
  entry that can never be fullfilled.  Needs investigation.

- Instead of the tag bit, have APs be prefixed with a size field.
  This trades off additional memory and AP creation for more efficient
  unwinding, update, and GC as well as giving bigger range to integers
  etc.  It may might also be possible to use that to update-in-place
  when the value is small than the original redex (AP).

  After that, use tags like INT x0 (31-/63-bit ints), AP 11, FUN 01.
  (11 allows x86 is use test 3, ..)

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
is an AP, containing a pointer to an application (a sequence of nodes)
on the heap, or an OTHER, containing something else.

  typedef enum {AP = 0, OTHER = 1} Tag;

The 2nd least-significant bit of a node is a flag stating whether or
not the node is the final node of an application.  Note, the final bit
is only allowed in the heap and must be stripped upon reading from the
heap so that the rest of the code can assume it's cleared.

> destType = "typedef enum dest Dest;"

If a node is an AP, its remaining 30 bits is a word-aligned heap
address.  Only use on a node that is known AP and has been stripped of
the Final bit (true for all values outside of the heap).

If the node is an OTHER, its 3rd least-significant bit contains a
sub-tag stating whether the the node is an INT or a FUN.

  typedef enum {INT = 0, FUN = 1} Subtag;

If a node is an INT, its remaining 29-bits is an unboxed integer.

If a node is a FUN, its remaining 29-bits contains a 6-bit arity and a
23-bit function identifier.

> macros = unlines
>   [ ""
>   , "static const Node TAGMASK    = 1;"
>   , "static const Node FINALMASK  = 2;"
>   , "static const Node SUBTAGMASK = 4 + 1/*TAGMASK*/;"
>   , ""
>   , "static const Node APTAG      = 1;"
>   , "static const Node INTTAG     = 0;"
>   , "static const Node FUNTAG     = 4/*SUBTAGMASK*/;"

In order to get faster dispatch, we pre-scale the index by the index
by the table stride and create a complicated macro gotoFUN to avoid
the scaling twice.

>   , "const Node FUNPOS     =12;"
>   , ""
>   , "static bool isAP(Node n)             {return (n&TAGMASK) == APTAG; }"
>   , "static Node *getAP(Node n)           {return(Node*)(n - APTAG);}"
>   , "static Node makeAP(Node *p,long f)   {return (Node)p + f*FINALMASK + APTAG;}"
>   , ""
>   , "static bool isFinal(Node n)          {return n &  FINALMASK; }"
>   , "static Node clearFinal(Node n)       {return n & ~FINALMASK; }"
>   , "static Node setFinal(Node n)         {return n |  FINALMASK; }"
>   , "static Node markFinal(Node n,long f) {return n + f*FINALMASK; }"
>   , "static Node copyFinal(Node n,Node m) {return n + (m&FINALMASK); }"

>   , "static bool isINT(Node n)            {return (n&SUBTAGMASK) == INTTAG;}"
>   , "static long getINT(Node n)           {return (long)n >> 3;}"
>   , "static Node makeINT(long i,long f)   {return (i << 3) + f*FINALMASK + INTTAG;}"

>   , "static bool isFUN(Node n)            {return (n&SUBTAGMASK) == FUNTAG;}"
>   , "static Node getARITY(Node n)         {return (n >> 3) & 63;}"
>   , "static Node getFUN(Node n)           {return n >> FUNPOS;}"
>   , "static const long LOGW = sizeof(uintptr_t) == 8 ? 3 : 2;"
>   , "#define gotoFUN(n,o) goto **(void **)((void *)funEntry + ((n) >> (FUNPOS-LOGW)) + ((o) << LOGW))"
>   , "static Node makeFUN(long arity, long fun, long f)"
>   , "                                     {return (fun << FUNPOS) + (arity << 3) + f*FINALMASK + FUNTAG;}"

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
>   , "  for (;;) {"
>   , "    top = *p++;"
>   , "    if (isFinal(top))"
>   , "        break;"
>   , "    *sp++ = top;"
>   , "  }"
>   , "  top = clearFinal(top);"
>   , "}"
>   ]

Updating
--------

The following code determines if a normal form has been reached, and
if so, performs an update.

> updateCode = unlines
>   [ "{"
>   , "  unsigned long ari = arity(top);"
>   , "  if (sp - ari < stack)"
>   , "    goto EXIT;"
>   , "  for (;;) {"
>   , "    unsigned long args = (unsigned long) (sp - usp->s);"
>   , "    if (ari <= args || usp <= ustack)"
>   , "      break;"
>   , "    Node *base = hp;"
>   , "    Node *p = sp - args;"
>   , "    while (p < sp) *hp++ = *p++;"
>   , "    *hp++ = setFinal(top);"
>   , "    *usp->h = makeAP(base, 1);"
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
>   , "} else"
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
>               "(.&.)", "st32", "ld32", "emit", "emitInt" ]

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
>       ++ "makeFUN(1," ++ fun "True"  ++ ",0) "
>       ++ ": makeFUN(1," ++ fun "False" ++ ",0);"
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
>   , "top = makeINT(getchar(),0);"
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
>   , st32Prim "st32"
>   , st32Prim "swap:st32"
>   , ld32Prim "ld32"
>   , ld32Prim "swap:ld32"
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
>   , "  *src = makeAP(dst,0);"
>   , "  return dst;"
>   , "}"
>   ]

> copyCode = unlines
>   [ "void copy() {"
>   , "  for (Node *low = toSpace; low < tsp; ++low) {"
>   , "    Node n = *low;"
>   , "    if (isAP(n)) {"
>   , "      Node *loc = copyAP(getAP(clearFinal(n)));"
>   , "      *low = makeAP(loc, isFinal(n));"
>   , "    }"
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
>   , "    if (isAP(n))"
>   , "       *p1 = makeAP(copyAP(getAP(n)), 0);"
>   , "    p1++;"
>   , "  }"
>   , "  if (isAP(top))"
>   , "       top = makeAP(copyAP(getAP(top)), 0);"
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
