/* =================================== */
/* REDUCERON EMULATOR WITH SPECULATIVE */
/* EVALUATION OF PRIMITIVE REDEXES     */
/* Matthew N                           */
/* 23 September 2009                   */
/* =================================== */

#include <stdio.h>
#include <stdlib.h>

/* Compile-time options */

#define MAXPUSH 8
#define APSIZE  6
#define MAXAPS  4
#define MAXLUTS 2
#define MAXREGS 8

#define MAXHEAPAPPS 32000
#define MAXSTACKELEMS 8000
#define MAXTEMPLATES 8000

#define NAMELEN 128

/* Types */

typedef int Bool;

typedef char Char;

typedef int Int;

typedef long long Long;

typedef enum { NUM, ARG, REG, VAR, CON, FUN, PRI } AtomTag;

typedef int Num;

typedef struct { Bool shared; Int index; } Reg;

typedef struct { Bool shared; Int index; } Arg;

typedef struct { Bool shared; Int id; } Var;

typedef struct { Int arity; Int index; } Con;

typedef struct { Bool original; Int arity; Int id; } Fun;

typedef enum { ADD, SUB, EQ, NEQ, LEQ, EMIT, EMITINT, SEQ } Prim;

typedef struct { Int arity; Bool swap; Prim id; } Pri;

typedef union
  {
    Num num;
    Reg reg;
    Arg arg;
    Var var;
    Con con;
    Fun fun;
    Pri pri;
  } AtomContents;

typedef struct { AtomTag tag; AtomContents contents; } Atom;

typedef Int Lut;

typedef enum { AP, CASE, PRIM, COLLECTED } AppTag;

typedef struct {
    AppTag tag;
    union { Bool normalForm; Lut lut; Int regId; } details;
    Int size;
    Atom atoms[APSIZE];
  } App;

typedef struct
  {
    Char name[NAMELEN];
    Int arity;
    Int numLuts;
    Lut luts[MAXLUTS];
    Int numPushs;
    Atom pushs[MAXPUSH];
    Int numApps;
    App apps[MAXAPS];
  } Template;

typedef struct { Int saddr; Int haddr; } Update;

const Atom falseAtom = {.tag = CON, .contents.con = {0, 0}};

const Atom trueAtom = {.tag = CON, .contents.con = {0, 1}};

const Atom mainAtom = {.tag = FUN, .contents.fun = {0, 0, 0}};

/* Globals */

App* heap;
App* heap2;
Atom* stack;
Update* ustack;
Lut* lstack;
Template* code;
Atom *registers;

Int hp, gcLow, gcHigh, sp, usp, lsp, end, gcCount;

Int numTemplates;

/* Profiling info */

Long swapCount, primCount, applyCount, unwindCount,
     updateCount, selectCount, prsCandidateCount, prsSuccessCount;

typedef struct
  {
    Bool seen;
    Int callCount;
  } ProfEntry;

ProfEntry *profTable;

/* Display profiling table */

void displayProfTable()
{
  Int i, j, ticksPerCall;
  printf("\nPROFILING TABLE:\n");
  printf("+----------------------------------+------+----------+\n");
  printf("| %-32s | %4s | %8s |\n", "FUNCTION", "SIZE", "%TIME");
  printf("+----------------------------------+------+----------+\n");
  for (i = 0; i < numTemplates; i++) {
    if (! profTable[i].seen) {
      ticksPerCall = 0;
      for (j = i; j < numTemplates; j++) {
        if (!strcmp(code[j].name,code[i].name)) {
          ticksPerCall++;
          profTable[j].seen = 1;
        }
      }
      printf("| %-32s |   %2i | %8.2f |\n",
        code[i].name, ticksPerCall,
        (100*(double)(profTable[i].callCount*ticksPerCall))/
        (double)applyCount);
    }
  }
  printf("+----------------------------------+------+----------+\n");
}

/* Dashing */

Atom dash(Bool sh, Atom a)
{
  if (a.tag == VAR) a.contents.var.shared = a.contents.var.shared || sh;
  return a;
}

App dashApp(Bool sh, App* app)
{
  Int i;
  for (i = 0; i < app->size; i++)
    app->atoms[i] = dash(sh, app->atoms[i]);
}

/* Unwinding */

inline Bool nf(App* app)
{
  return (app->tag == CASE ? 0 : app->details.normalForm);
}

void pushAtoms(Int size, Atom* atoms)
{
  Int i;
  for (i = size-1; i >= 0; i--) stack[sp++] = atoms[i];
}

void unwind(Bool sh, Int addr)
{
  App app = heap[addr];
  if (sh && !nf(&app)) {
    Update u; u.saddr = sp-1; u.haddr = addr;
    ustack[usp++] = u;
  }
  dashApp(sh, &app);
  if (app.tag == CASE) lstack[lsp++] = app.details.lut;
  sp--;
  pushAtoms(app.size, app.atoms);
}

/* Updating */

inline Int arity(Atom a)
{
  switch (a.tag) {
    case NUM: return 1;
    case CON: return a.contents.con.arity+1;
    case PRI: return a.contents.pri.arity;
    case FUN: return a.contents.fun.arity;
    default: error(printf("arity(): invalid tag\n"));
  }
}

Bool updateCheck(Atom top, Update utop)
{
  return (arity(top) > sp-1 - utop.saddr);
}

void upd(Atom top, Int sp, Int len, Int hp)
{
  Int i, j;
  heap[hp].tag = AP;
  heap[hp].details.normalForm = 1;
  heap[hp].size = len;
  heap[hp].atoms[0] = top;
  for (i = 1, j = sp; i < len; i++, j--) {
    Atom a = dash(1, stack[j]);
    heap[hp].atoms[i] = a;
    stack[j] = a;
  }
}

void update(Atom top, Int saddr, Int haddr)
{
  Int len = sp - saddr;
  Int p = sp-2;

  Int i, j;

  for (;;) {
    if (len < APSIZE) {
      upd(top, p, len, haddr);
      usp--;
      return;
    }
    else {
      upd(top, p, APSIZE, hp);
      p -= APSIZE-1; len -= APSIZE-1;
      top.tag = VAR; top.contents.var.shared = 1; top.contents.var.id = hp;
      hp++;
    }
  }
}

/* Swapping */

void swap()
{
  Atom tmp = stack[sp-1];
  stack[sp-1] = stack[sp-2];
  stack[sp-2] = tmp;
}

/* Primitive reduction */

Atom prim(Prim p, Atom a, Atom b)
{
  Atom result;
  Num n, m;
  n = a.contents.num;
  m = b.contents.num;
  switch(p) {
    case ADD: result.tag = NUM; result.contents.num = n+m; break;
    case SUB: result.tag = NUM; result.contents.num = n-m; break;
    case EQ: result = n == m ? trueAtom : falseAtom; break;
    case NEQ: result = n != m ? trueAtom : falseAtom; break;
    case LEQ: result = n <= m ? trueAtom : falseAtom; break;
    case EMIT: printf("%c", n); result = b; break;
    case EMITINT: printf("%i", n); result = b; break;
  }
  return result;
}

void applyPrim()
{
  Pri p = stack[sp-2].contents.pri;
  if (p.id == SEQ) {
    stack[sp-2] = stack[sp-3];
    stack[sp-3] = stack[sp-1];
    sp-=1;
    primCount++;
  }
  else if (stack[sp-3].tag == NUM
        || p.id == EMIT || p.id == EMITINT) {
    if (p.swap == 1)
      stack[sp-3] = prim(p.id, stack[sp-3], stack[sp-1]);
    else
      stack[sp-3] = prim(p.id, stack[sp-1], stack[sp-3]);
    sp-=2;
    primCount++;
  }
  else {
      Atom tmp;
      stack[sp-2].contents.pri.swap = !stack[sp-2].contents.pri.swap;
      tmp = stack[sp-1];
      stack[sp-1] = stack[sp-3];
      stack[sp-3] = tmp;
      swapCount++;
  }
}

/* Case-alt selection */

void caseSelect(Int index)
{
  Int lut = lstack[lsp-1];
  stack[sp-1].tag = FUN;
  stack[sp-1].contents.fun.original = 1;
  stack[sp-1].contents.fun.arity = 0;
  stack[sp-1].contents.fun.id = lut+index;
  lsp--;
}

/* Function application */

Atom inst(Int base, Int argPtr, Atom a)
{
  if (a.tag == VAR) {
    a.contents.var.id = base + a.contents.var.id;
  }
  else if (a.tag == ARG) {
    a = dash(a.contents.arg.shared, stack[argPtr-a.contents.arg.index]);
  }
  else if (a.tag == REG) {
    a = dash(a.contents.reg.shared, registers[a.contents.reg.index]);
  }
  return a;
}

Atom getPrimArg(Int argPtr, Atom a)
{
  if (a.tag == ARG) return stack[argPtr-a.contents.arg.index];
  else if (a.tag == REG) return registers[a.contents.reg.index];
  else return a;
}

void instApp(Int base, Int argPtr, App *app)
{
  Int i;
  Atom a, b;
  App* new = &heap[hp];
  Int rid;

  if (app->tag == PRIM) {
    prsCandidateCount++;
    a = app->atoms[0]; b = app->atoms[2];
    a = getPrimArg(argPtr, a);
    b = getPrimArg(argPtr, b);
    rid = app->details.regId;
    if (a.tag == NUM && b.tag == NUM) {
      prsSuccessCount++;
      registers[rid] = prim(app->atoms[1].contents.pri.id, a, b);
    }
    else {
      registers[rid].tag = VAR;
      registers[rid].contents.var.shared = 0;
      registers[rid].contents.var.id = hp;
      new->tag = AP;
      new->details.normalForm = 0;
      new->size = app->size;
      for (i = 0; i < app->size; i++)
        new->atoms[i] = inst(base, argPtr, app->atoms[i]);
      hp++;
    }
  }
  else {
    new->tag = app->tag;
    new->size = app->size;
    for (i = 0; i < app->size; i++)
      new->atoms[i] = inst(base, argPtr, app->atoms[i]);
    if (app->tag == CASE) new->details.lut = app->details.lut;
    if (app->tag == AP) new->details.normalForm = app->details.normalForm;
    hp++;
  }
}

void slide(Int p, Int n)
{
  Int i;
  for (i = p; i < sp; i++) stack[i-n] = stack[i];
  sp -= n;
}

void apply(Template* t)
{
  Int i;
  Int base = hp;
  Int spOld = sp;

  for (i = t->numLuts-1; i >= 0; i--) lstack[lsp++] = t->luts[i];
  for (i = 0; i < t->numApps; i++)
    instApp(base, spOld-2, &(t->apps[i]));
  for (i = t->numPushs-1; i >= 0; i--)
    stack[sp++] = inst(base, spOld-2, t->pushs[i]);

  slide(spOld, t->arity+1);
}

/* Garbage collection */

Bool isSimple(App *app)
{
  return (app->size == 1 && app->tag != CASE &&
           (app->atoms[0].tag == NUM || app->atoms[0].tag == CON));
}

Atom copyChild(Atom child)
{
  App app;
  if (child.tag == VAR) {
    app = heap[child.contents.var.id];
    if (app.tag == COLLECTED || isSimple(&app)) {
      return app.atoms[0];
    }
    else {
      Int addr = child.contents.var.id;
      child.contents.var.id = gcHigh;
      heap[addr].tag = COLLECTED;
      heap[addr].size = 1;
      heap[addr].atoms[0] = child;
      heap2[gcHigh++] = app;
      return child;
    }
  }
  return child;
}

void copy()
{
  Int i;
  App app;
  while (gcLow < gcHigh) {
    app = heap2[gcLow];
    for (i = 0; i < app.size; i++)
      app.atoms[i] = copyChild(app.atoms[i]);
    heap2[gcLow++] = app;
  }
}

void updateUStack()
{
  Int i, j;
  App app;
  for (i = 0, j = 0; i < usp; i++) {
    app = heap[ustack[i].haddr];
    if (app.tag == COLLECTED) {
      ustack[j].saddr = ustack[i].saddr;
      ustack[j].haddr = app.atoms[0].contents.var.id;
      j++;
    }
  }
  usp = j;
}

void collect()
{
  Int i;
  App* tmp;
  gcCount++;
  gcLow = gcHigh = 0;
  for (i = 0; i < sp; i++) stack[i] = copyChild(stack[i]);
  copy();
  updateUStack();
  tmp = heap; heap = heap2; heap2 = tmp;
  hp = gcHigh;
  //printf("After GC: %i\n", hp);
}

/* Allocate memory */

void alloc()
{
  heap = (App*) malloc(sizeof(App) * MAXHEAPAPPS);
  heap2 = (App*) malloc(sizeof(App) * MAXHEAPAPPS);
  stack = (Atom*) malloc(sizeof(Atom) * MAXSTACKELEMS);
  ustack = (Update*) malloc(sizeof(Update) * MAXSTACKELEMS);
  lstack = (Lut*) malloc(sizeof(Lut) * MAXSTACKELEMS);
  code = (Template*) malloc(sizeof(Template) * MAXTEMPLATES);
  registers = (Atom*) malloc(sizeof(Atom) * MAXREGS);
  profTable = (ProfEntry*) malloc(sizeof(ProfEntry) * MAXTEMPLATES);
}

/* Initialise globals */

void initProfTable()
{
  Int i;
  for (i = 0; i < MAXTEMPLATES; i++) {
    profTable[i].seen = 0;
    profTable[i].callCount = 0;
  }
}

void init()
{
  sp = 1;
  usp = lsp = hp = 0;
  stack[0] = mainAtom;
  swapCount = primCount = applyCount =
    unwindCount = updateCount = selectCount = 
      prsCandidateCount = prsSuccessCount = gcCount = 0;
  initProfTable();
}

/* Dispatch loop */

inline Bool canCollect()
{
  return (stack[sp-1].tag != FUN || stack[sp-1].contents.fun.original);
}

void stackOverflow()
{
  printf("Out of stack space.\n");
  exit(-1);
}

void dispatch()
{
  Atom top;

  while (!(sp == 1 && stack[0].tag == NUM)) {
    if (sp > MAXSTACKELEMS-100) stackOverflow();
    if (usp > MAXSTACKELEMS-100) stackOverflow();
    if (lsp > MAXSTACKELEMS-100) stackOverflow();
    if (hp > MAXHEAPAPPS-200 && canCollect()) collect();
    top = stack[sp-1];
    if (top.tag == VAR) {
      unwind(top.contents.var.shared, top.contents.var.id);
      unwindCount++;
    }
    else if (usp > 0 && updateCheck(top, ustack[usp-1])) {
      update(top, ustack[usp-1].saddr, ustack[usp-1].haddr);
      updateCount++;
    }
    else {
      switch (top.tag) {
        case NUM: if (stack[sp-2].tag == PRI) applyPrim(); else swap();  break;
        case FUN: profTable[top.contents.fun.id].callCount++; applyCount++;
                  apply(&code[top.contents.fun.id]); break;
        case CON: selectCount++; caseSelect(top.contents.con.index); break;
        default: error(printf("dispatch(): invalid tag.\n")); break; 
      }
    }
  }
}

/* Parser for .red files */

#define perform(action) (action, 1)

#define error(action) (action, exit(-1), 0)

Int strToBool(Char *s)
{
  if (!strcmp(s, "True")) return 1;
  if (!strcmp(s, "False")) return 0;
  error(printf("Parse error: boolean expected; got %s\n", s));
}

void strToPrim(Char *s, Prim *p, Bool *b)
{
  *b = 0;
  if (!strcmp(s, "emit")) { *p = EMIT; return; }
  if (!strcmp(s, "emitInt")) { *p = EMITINT; return; }
  if (!strcmp(s, "(!)")) { *p = SEQ; return; }
  if (!strncmp(s, "swap:", 5)) {
    *b = 1;
    s = s+5;
  }
  if (!strcmp(s, "(+)")) { *p = ADD; return; }
  if (!strcmp(s, "(-)")) { *p = SUB; return; }
  if (!strcmp(s, "(==)")) { *p = EQ; return; }
  if (!strcmp(s, "(/=)")) { *p = NEQ; return; }
  if (!strcmp(s, "(<=)")) { *p = LEQ; return; }
  error(printf("Parse error: unknown primitive %s\n", s));
}

Bool parseAtom(Atom* result)
{
  Char str[16];

  return (
    (  scanf(" INT%*[ (]%i)", &result->contents.num) == 1
    && perform(result->tag = NUM)
    )
    ||
    (  scanf(" ARG %5s%*[ (]%i)", str, &result->contents.arg.index) == 2
    && perform(result->tag = ARG)
    && perform(result->contents.arg.shared = strToBool(str))
    )
    ||
    (  scanf(" VAR %5s%*[ (]%i)", str, &result->contents.var.id) == 2
    && perform(result->tag = VAR)
    && perform(result->contents.var.shared = strToBool(str))
    )
    ||
    (  scanf(" REG %5s%*[ (]%i)", str, &result->contents.reg.index) == 2
    && perform(result->tag = REG)
    && perform(result->contents.reg.shared = strToBool(str))
    )
    ||
    (  scanf(" CON%*[ (]%i%*[ )(]%i)",
         &result->contents.con.arity, &result->contents.con.index) == 2
    && perform(result->tag = CON)
    )
    ||
    (  scanf(" FUN %5s%*[ (]%i%*[ )(]%i)",
         str, &result->contents.fun.arity, &result->contents.fun.id) == 3
    && perform(result->tag = FUN)
    && perform(result->contents.fun.original = strToBool(str))
    )
    ||
    (  scanf(" PRI%*[ (]%i%*[) ]\"%10[^\"]\"", &result->contents.pri.arity, str)
    && perform(result->tag = PRI)
    && perform(strToPrim(str, &result->contents.pri.id,
                              &result->contents.pri.swap))
    ) 
  );
}

#define makeListParser(f, p, elem)                                          \
  Int f(Int n, elem *xs)                                                    \
    {                                                                       \
      Char c;                                                               \
      Int i = 0;                                                            \
      if (! (scanf(" %c", &c) == 1 && c == '['))                            \
        error(printf("Parse error: expecting '['\n"));                      \
      for (;;) {                                                            \
        if (i >= n)                                                         \
          error(printf("Parse error: list contains too many elements\n"));  \
        if (p(&xs[i])) i++;                                                 \
        if (scanf(" %c", &c) == 1 && (c == ',' || c == ']')) {              \
          if (c == ']') return i;                                           \
        }                                                                   \
        else error(printf("Parse error\n"));                                \
      }                                                                     \
      return 0;                                                             \
    }

makeListParser(parseAtoms, parseAtom, Atom)

Bool parseApp(App *app)
{
  Int i;
  Char str[16];
  Bool success;
  success =
    (  scanf(" APP %5s ", str) == 1
    && perform(app->tag = AP)
    && perform(app->details.normalForm = strToBool(str))
    )
    ||
    (  scanf(" CASE %i ", &app->details.lut) == 1
    && perform(app->tag = CASE)
    )
    ||
    (  scanf(" PRIM %i ", &app->details.regId) == 1
    && perform(app->tag = PRIM)
    );
  if (!success) return 0;
  app->size = parseAtoms(APSIZE, app->atoms);
  return 1;
}

makeListParser(parseApps, parseApp, App)

Bool parseLut(Int *i)
{
  return (scanf(" %i", i) == 1);
}

makeListParser(parseLuts, parseLut, Lut)

Bool parseString(Int n, Char *str)
{
  Int i;
  Char c;
  scanf(" \"");
  
  for (i = 0; ; i++) {
    if (i >= n) return 0;
    scanf("%c", &c);
    if (c == '"') {
      str[i] = '\0';
      return 1;
    }
    str[i] = c;
  }
}

Bool parseTemplate(Template *t)
{
  Char c;
  scanf(" (");
  if (parseString(NAMELEN, t->name) == 0) return 0;
  if (scanf(" ,%i,", &t->arity) != 1) return 0;
  t->numLuts = parseLuts(MAXLUTS, t->luts);
  (scanf(" %c", &c) == 1 && c == ',') || error(printf("Parse error\n"));
  t->numPushs = parseAtoms(MAXPUSH, t->pushs);
  (scanf(" %c", &c) == 1 && c == ',') || error(printf("Parse error\n"));
  t->numApps = parseApps(MAXAPS, t->apps);
  (scanf(" %c", &c) == 1 && c == ')') || error(printf("Parse error\n"));
  return 1;
}

Int parse(Int n, Template *ts)
{
  Int i = 0;

  for (;;) {
    if (i >= n) error(printf("Parse error: too many templates\n"));
    if (!parseTemplate(&ts[i])) return i;
    i++;
  }
}

/* Main function */

int main()
{
  Long ticks;

  alloc();
  numTemplates = parse(MAXTEMPLATES, code);
  if (numTemplates <= 0) error(printf("No templates were parsed!\n"));
  init();
  dispatch();

  printf("\n==== EXECUTION REPORT ====\n");
  printf("Result      = %12i\n", stack[0].contents.num);
  ticks = swapCount + primCount + applyCount +
            unwindCount + updateCount;
  printf("Ticks       = %12lld\n", ticks);
  printf("Swap        = %11lld%%\n", (100*swapCount)/ticks);
  printf("Prim        = %11lld%%\n", (100*primCount)/ticks);
  printf("Unwind      = %11lld%%\n", (100*unwindCount)/ticks);
  printf("Update      = %11lld%%\n", (100*updateCount)/ticks);
  printf("Apply       = %11lld%%\n", (100*applyCount)/ticks);
  printf("PRS Success = %11lld%%\n",
    (100*prsSuccessCount)/(1+prsCandidateCount));
  printf("#GCs        = %12lld\n", gcCount);
  printf("==========================\n");

  displayProfTable();

  return 0;
}
