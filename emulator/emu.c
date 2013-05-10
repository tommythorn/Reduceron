/* =================================== */
/* REDUCERON EMULATOR WITH SPECULATIVE */
/* EVALUATION OF PRIMITIVE REDEXES     */
/* Matthew N                           */
/* 23 September 2009                   */
/* =================================== */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <getopt.h>

/* Compile-time options */

#define MAXPUSH 8
#define APSIZE  6
#define MAXAPS  4
#define MAXLUTS 2
#define MAXREGS 8

#define MAXHEAPAPPS    8192
#define MAXSTACKELEMS  1024
#define MAXUSTACKELEMS 512
#define MAXLSTACKELEMS 512
#define MAXTEMPLATES   1024

#define NAMELEN 128

//#define ONEBITGC_STUDY1  1

/* Simulate the limited integer range (anything less than 18 and
 * CountDown breaks, anything less than 22 and While breaks). */
#define ATOMWIDTH 22
#define TRUNCATE(x) ((int) (x) << (35 - ATOMWIDTH) >> (35 - ATOMWIDTH))

#define perform(action) (action, 1)

#define error(msg ...) (fprintf(stderr,"At step #%d, ", stepno), fprintf(stderr,msg), assert(0), 0)

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

typedef enum { ADD, SUB, EQ, NEQ, LEQ, EMIT, EMITINT, SEQ, AND, ST32, LD32, LAST_PRIM } Prim;

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

typedef enum { AP, CASE, PRIM, COLLECTED, INVALID } AppTag;

typedef struct {
    AppTag tag;
    Int refcnt; // Used and updated by GC
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

const Atom mainAtom = {.tag = FUN, .contents.fun = {1, 0, 0}};

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

Int maxHeapUsage, maxStackUsage, maxUStackUsage, maxLStackUsage;

Bool tracingEnabled = 0;
int stepno = 0;

#if ONEBITGC_STUDY1
Long sumCollected, sumOneBitCollected;
double sumGc1bitpart = 0.0;
#endif

typedef struct
  {
    Bool seen;
    Int callCount;
  } ProfEntry;

ProfEntry *profTable;

void stackOverflow(const char *);
void integerAddOverflow(int a, int b);

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

#if ONEBITGC_STUDY1
void collectApp(Int addr)
{
    // Corrupt the app
    memset(&heap[addr], 0xEE, sizeof heap[addr]);
    heap[addr].tag = INVALID;
    ++sumOneBitCollected;
}
#endif

void showAtom(Atom);

static void refcntcheck(Atom a)
{
    if (a.tag == VAR && !a.contents.var.shared &&
        heap[a.contents.var.id].refcnt > 1) {
        fprintf(stderr, "Uniqueness violation: ");
        showAtom(a);
        fprintf(stderr, "\n");
        assert(0);
    }
}

/* Dashing */

Atom dash(Bool sh, Atom a)
{
  if (a.tag == VAR) a.contents.var.shared = a.contents.var.shared || sh;

  return a;
}

void dashApp(Bool sh, App* app)
{
  Int i;

  if (app->tag >= INVALID)
      error("dashApp(): invalid tag.\n");

  for (i = 0; i < app->size; i++)
    app->atoms[i] = dash(sh, app->atoms[i]);
}

/* Unwinding */

static inline Bool nf(App* app)
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

  if (app.tag >= INVALID)
      error("unwind(): invalid tag.\n");

  if (sh && !nf(&app)) {
    Update u; u.saddr = sp-1; u.haddr = addr;
    ustack[usp++] = u;
  }
#if ONEBITGC_STUDY1
  if (!sh)
    collectApp(addr);
#endif
  dashApp(sh, &app);
  if (app.tag == CASE) lstack[lsp++] = app.details.lut;
  sp--;
  pushAtoms(app.size, app.atoms);
}

/* Updating */

static inline Int arity(Atom a)
{
  switch (a.tag) {
    case NUM: return 1;
    case CON: return a.contents.con.arity+1;
    case PRI: return a.contents.pri.arity;
    case FUN: return a.contents.fun.arity;
    default: error("arity(): invalid tag\n");
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

Atom prim_ld32(Num addr)
{
    /* For now, a quick hack:
       [0] - serial in
    */
    Atom res = { .tag = NUM, .contents.num = 666 };

    if (addr == 0)
        res.contents.num = getchar();

    if (tracingEnabled) {
        printf("[[ld32 (%d) -> %d]]", addr, res.contents.num);
        fflush(stdout);
    }

    /* This is a hack to terminate otherwise infinite processes */
    if (res.contents.num < 0)
        exit(0);

    return res;
}

Atom prim_st32(Num addr, Num value, Atom k)
{
    /* For now, a quick hack:
       [0] - serial out
    */

    if (addr == 0)
        putchar(value);

    if (tracingEnabled) {
        printf("[[st32 (%d)=%d]]", addr, value);
        fflush(stdout);
    }

    return k;
}


Atom prim(Prim p, Atom a, Atom b, Atom c)
{
  Atom result;
  Num n, m;
  n = a.contents.num;
  m = b.contents.num;
  switch (p) {
    case ADD:
        result.tag = NUM;
        result.contents.num = TRUNCATE(n+m);
        if (result.contents.num != n+m)
            integerAddOverflow(n, m);
        break;
    case SUB:
        result.tag = NUM;
        result.contents.num = TRUNCATE(n-m);
        if (result.contents.num != n-m)
            integerAddOverflow(n, -m);
        break;
    case EQ: result = n == m ? trueAtom : falseAtom; break;
    case NEQ: result = n != m ? trueAtom : falseAtom; break;
    case LEQ: result = n <= m ? trueAtom : falseAtom; break;
    case EMIT: printf("%c", n); fflush(stdout); result = b; break;
    case EMITINT: printf("%i", n); fflush(stdout); result = b; break;
    case AND: result.tag = NUM; result.contents.num = TRUNCATE(n&m); break;
    case ST32: result = prim_st32(n, m, c); break;
    case LD32: result = prim_ld32(n); break;
    case SEQ: assert(0);
    case LAST_PRIM: assert(0);
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
        stack[sp-3] = prim(p.id, stack[sp-3], stack[sp-1], stack[4 <= sp ? sp-4 : 0]);
    else
        stack[sp-3] = prim(p.id, stack[sp-1], stack[sp-3], stack[4 <= sp ? sp-4 : 0]);
    sp -= p.arity;
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

  if (app->tag >= INVALID)
      error("instApp(): invalid tag.\n");

  if (app->tag == PRIM) {
    prsCandidateCount++;
    a = app->atoms[0]; b = app->atoms[2];
    a = getPrimArg(argPtr, a);
    b = getPrimArg(argPtr, b);
    rid = app->details.regId;
    if (a.tag == NUM && b.tag == NUM) {
      prsSuccessCount++;
      registers[rid] = prim(app->atoms[1].contents.pri.id, a, b, b);
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
    if (app->tag >= INVALID)
      error("dashApp(): invalid tag.\n");

  return (app->size == 1 && app->tag != CASE &&
           (app->atoms[0].tag == NUM || app->atoms[0].tag == CON));
}

Atom copyChild(Atom child)
{
  App app;
  if (child.tag == VAR) {
    app = heap[child.contents.var.id];

    if (app.tag >= INVALID)
      error("copyChild(): invalid tag.\n");

    if (app.tag == COLLECTED)
        ++heap2[app.atoms[0].contents.var.id].refcnt;

    if (isSimple(&app))
      return app.atoms[0];

    if (app.tag == COLLECTED) {
      // return app.atoms[0];
      // This was a bug, as this would ignore the value
      // of the childs shared attribute
      child.contents.var.id = app.atoms[0].contents.var.id;
    }
    else {
      Int addr = child.contents.var.id;
      child.contents.var.id = gcHigh;
      heap[addr].tag = COLLECTED;
      heap[addr].size = 1;
      heap[addr].atoms[0] = child;
      heap2[gcHigh] = app;
      heap2[gcHigh++].refcnt = 1;
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

    if (app.tag >= INVALID)
      error("copy(): invalid tag.\n");

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

    if (app.tag >= INVALID)
      error("updateUStack(): invalid tag.\n");

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

#ifdef ONEBITGC_STUDY1
  double gcPercent     = 100.0 * (hp - gcHigh) / hp;
  double gcPercent1bit = 100.0 * sumOneBitCollected / hp;
  double gc1bitpart    = gcPercent1bit / gcPercent;

  if (0)
  fprintf(stderr,
          "GC #%4d: collected %4d apps out of %4d (%5.1f%%) "
          "OBRC %4lld (%5.1f%%), thus %5.1f%% of the garbage\n",
          gcCount, hp - gcHigh, hp, gcPercent,
          sumOneBitCollected, gcPercent1bit,
          100.0 * gc1bitpart);

  sumOneBitCollected = 0;
  sumGc1bitpart += gc1bitpart;
#endif

  hp = gcHigh;

  if (hp > maxHeapUsage) maxHeapUsage = hp;
  if (hp > MAXHEAPAPPS-200) stackOverflow("heap");
}

/* Allocate memory */

void alloc()
{
  heap = (App*) malloc(sizeof(App) * MAXHEAPAPPS);
  heap2 = (App*) malloc(sizeof(App) * MAXHEAPAPPS);
  stack = (Atom*) malloc(sizeof(Atom) * MAXSTACKELEMS);
  ustack = (Update*) malloc(sizeof(Update) * MAXUSTACKELEMS);
  lstack = (Lut*) malloc(sizeof(Lut) * MAXLSTACKELEMS);
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

static inline Bool canCollect()
{
  return (stack[sp-1].tag != FUN || stack[sp-1].contents.fun.original);
}

void stackOverflow(const char *which)
{
    printf("%s is out of space (hp = %d, sp = %d, usp = %d, lsp = %d).\n",
           which, hp, sp, usp, lsp);
    exit(-1);
}

void integerAddOverflow(int a, int b)
{
    printf("Integer range exhausted in addition of %d+%d = %d\n",
           a, b, a+b);
    exit(-1);
}

/* Succint printing of Atoms and Apps:
   - C2___ for CON 2, arity 3
   - F3__ for FUN 3, arity 2

   - a5 for ARG 5, a5{1} for unique (= unshared)
   - r7 for regs, r7{1} for unique (= unshared)
 */

/* returns a string of as many _ as n (for n <= 16) */
char *arityStr(int n)
{
    return "________________" + 16 - n;
}

/* Use . postfix for unique pointers */
char *shareStr(int sh)
{
    return sh ? "" : ".";
}

void showAtom(Atom a)
{
    static const char *primName[] = {
        "+",
        "-",
        "==",
        "!=",
        "<=",
        "emit",
        "emitInt",
        "!",
        ".&.",
        "st32",
        "ld32",
    };

    switch (a.tag) {
    case NUM: printf("%d", a.contents.num); break;
    case ARG: printf("a%d%s", a.contents.arg.index, shareStr(a.contents.arg.shared)); break;
    case REG: printf("r%d%s", a.contents.reg.index, shareStr(a.contents.reg.shared)); break;
    case VAR: printf("h%d%s", a.contents.var.id,    shareStr(a.contents.var.shared)); break;
    case CON: printf("C%d%s", a.contents.con.index, arityStr(a.contents.con.arity)); break;
    case FUN: printf("F%d%s%s", a.contents.fun.id,  arityStr(a.contents.fun.arity),
                     a.contents.fun.original ? "" : ".."); break;
    case PRI: printf("%s%s", a.contents.pri.swap ? "*:" : "",
                     a.contents.pri.id < LAST_PRIM ? primName[a.contents.pri.id] : "?"); break;
    default: assert(0);
    }
}

void showApp(int addr)
{
    App app = heap[addr];

    //printf("h%d{%d}:", addr, app.refcnt);
    printf("h%d:", addr);

    switch (app.tag) {
    case AP: printf("("); break;
    case CASE: printf("(CASE F%d ", app.details.lut); break;
    case PRIM: printf("(r%d=", app.details.regId); break;
    case COLLECTED:printf("COLLECTED"); return;
    case INVALID: printf("INVALID"); return;
    default: assert(0);
    }

    for (int i = 0; i < app.size; ++i) {
        if (i)
            putchar(' ');
        showAtom(app.atoms[i]);
    }
    printf(")");
}

void dispatch()
{
  Atom top;

  while (!(sp == 1 && stack[0].tag == NUM)) {
      if (sp > maxStackUsage) maxStackUsage = sp;
      if (usp > maxUStackUsage) maxUStackUsage = usp;
      if (lsp > maxLStackUsage) maxLStackUsage = lsp;

    if (sp > MAXSTACKELEMS-50) stackOverflow("stack");
    if (usp > MAXUSTACKELEMS-4) stackOverflow("update stack");
    if (lsp > MAXLSTACKELEMS-4) stackOverflow("case stack");
    if (hp > MAXHEAPAPPS-200 && canCollect()) collect();

    /* Trace */

    if (tracingEnabled) {
        printf("\nStep #%d\n", stepno);
        printf("Heap  :");
        for (int i = 0; i < hp; ++i)
            if (heap[i].tag < COLLECTED) {
                putchar(' ');
                showApp(i);
            }
        printf("\n");
        printf("Stack :");
        for (int i = sp - 1; i >= 0; --i) {
            putchar(' ');
            showAtom(stack[i]);
        }
        printf("\n");

        printf("Regs  :");
        for (int i = 0; i < MAXREGS; ++i) {
            putchar(' ');
            showAtom(registers[i]);
        }
        printf("\n");

        for (int i = 0; i < MAXREGS; ++i)
            refcntcheck(registers[i]);
        for (int i = 0; i < sp; ++i)
            refcntcheck(stack[i]);
    }

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
        default: error("dispatch(): invalid tag.\n"); break;
      }
    }

    ++stepno;
  }
}

/* Parser for .red files */

Int strToBool(Char *s)
{
  if (!strcmp(s, "True")) return 1;
  if (!strcmp(s, "False")) return 0;
  error("Parse error: boolean expected; got %s\n", s);
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
  if (!strcmp(s, "(.&.)")) { *p = AND; return; }
  if (!strcmp(s, "st32")) { *p = ST32; return; }
  if (!strcmp(s, "ld32")) { *p = LD32; return; }
  error("Parse error: unknown primitive %s\n", s);
}

Bool parseAtom(FILE *f, Atom* result)
{
  Char str[16];

  return (
    (  fscanf(f, " INT%*[ (]%i)", &result->contents.num) == 1
    && perform(result->contents.num = TRUNCATE(result->contents.num))
    && perform(result->tag = NUM)
    )
    ||
    (  fscanf(f, " ARG %5s%*[ (]%i)", str, &result->contents.arg.index) == 2
    && perform(result->tag = ARG)
    && perform(result->contents.arg.shared = strToBool(str))
    )
    ||
    (  fscanf(f, " VAR %5s%*[ (]%i)", str, &result->contents.var.id) == 2
    && perform(result->tag = VAR)
    && perform(result->contents.var.shared = strToBool(str))
    )
    ||
    (  fscanf(f, " REG %5s%*[ (]%i)", str, &result->contents.reg.index) == 2
    && perform(result->tag = REG)
    && perform(result->contents.reg.shared = strToBool(str))
    )
    ||
    (  fscanf(f, " CON%*[ (]%i%*[ )(]%i)",
         &result->contents.con.arity, &result->contents.con.index) == 2
    && perform(result->tag = CON)
    )
    ||
    (  fscanf(f, " FUN %5s%*[ (]%i%*[ )(]%i)",
         str, &result->contents.fun.arity, &result->contents.fun.id) == 3
    && perform(result->tag = FUN)
    && perform(result->contents.fun.original = strToBool(str))
    )
    ||
    (  fscanf(f, " PRI%*[ (]%i%*[) ]\"%10[^\"]\"", &result->contents.pri.arity, str)
    && perform(result->tag = PRI)
    && perform(strToPrim(str, &result->contents.pri.id,
                              &result->contents.pri.swap))
    )
  );
}

#define makeListParser(fun, p, elem)                                        \
  Int fun(FILE *f, Int n, elem *xs)                                         \
    {                                                                       \
      Char c;                                                               \
      Int i = 0;                                                            \
      if (! (fscanf(f, " %c", &c) == 1 && c == '['))                        \
        error("Parse error: expecting '['\n");                              \
      for (;;) {                                                            \
        if (i >= n)                                                         \
          error("Parse error: list contains too many elements\n");          \
        if (p(f, &xs[i])) i++;                                              \
        if (fscanf(f, " %c", &c) == 1 && (c == ',' || c == ']')) {          \
          if (c == ']') return i;                                           \
        }                                                                   \
        else error("Parse error\n");                                        \
      }                                                                     \
      return 0;                                                             \
    }

makeListParser(parseAtoms, parseAtom, Atom)

Bool parseApp(FILE *f, App *app)
{
  Char str[16];
  Bool success =
    (  fscanf(f, " APP %5s ", str) == 1
    && perform(app->tag = AP)
    && perform(app->details.normalForm = strToBool(str))
    )
    ||
    (  fscanf(f, " CASE %i ", &app->details.lut) == 1
    && perform(app->tag = CASE)
    )
    ||
    (  fscanf(f, " PRIM %i ", &app->details.regId) == 1
    && perform(app->tag = PRIM)
    );
  if (!success) return 0;
  app->size = parseAtoms(f, APSIZE, app->atoms);
  return 1;
}

makeListParser(parseApps, parseApp, App)

Bool parseLut(FILE *f, Int *i)
{
    return fscanf(f, " %i", i) == 1;
}

makeListParser(parseLuts, parseLut, Lut)

Bool parseString(FILE *f, Int n, Char *str)
{
  Int i;
  Char c;
  fscanf(f, " \"");

  for (i = 0; ; i++) {
    if (i >= n) return 0;
    fscanf(f, "%c", &c);
    if (c == '"') {
      str[i] = '\0';
      return 1;
    }
    str[i] = c;
  }
}

Bool parseTemplate(FILE *f, Template *t)
{
  Char c;
  fscanf(f, " (");
  if (parseString(f, NAMELEN, t->name) == 0) return 0;
  if (fscanf(f, " ,%i,", &t->arity) != 1) return 0;
  t->numLuts = parseLuts(f, MAXLUTS, t->luts);
  (fscanf(f, " %c", &c) == 1 && c == ',') || error("Parse error\n");
  t->numPushs = parseAtoms(f, MAXPUSH, t->pushs);
  (fscanf(f, " %c", &c) == 1 && c == ',') || error("Parse error\n");
  t->numApps = parseApps(f, MAXAPS, t->apps);
  (fscanf(f, " %c", &c) == 1 && c == ')') || error("Parse error\n");
  return 1;
}

Int parse(FILE *f, Int n, Template *ts)
{
  Int i = 0;

  for (;;) {
    if (i >= n) error("Parse error: too many templates\n");
    if (!parseTemplate(f, &ts[i])) return i;
    i++;
  }
}

/* Main function */

int main(int argc, char *argv[])
{
  FILE *f;
  Long ticks;
  int ch;
  Bool verbose = 0;
  Bool profiling = 0;

  while ((ch = getopt(argc, argv, "vtp")) != -1) {
      switch (ch) {
      case 'v':
          verbose = 1;
          break;
      case 't':
          tracingEnabled = 1;
          break;
      case 'p':
          profiling = 1;
          break;
      default:
          error("only options v, t and p supported\n");
          break;
      }
  }

  argc -= optind;
  argv += optind;

  if (argc != 1)
      error("Need .red file or - for stdin");

  if (strcmp(argv[0], "-") == 0)
      f = stdin;
  else
      f = fopen(argv[0], "r");

  if (!f) {
      perror(argv[0]);
      exit(-1);
  }

  alloc();
  numTemplates = parse(f, MAXTEMPLATES, code);
  if (numTemplates <= 0) error("No templates were parsed!\n");
  init();
  dispatch();

  if (verbose) {
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
      printf("#GCs        = %12d\n", gcCount);
      printf("Max Heap    = %12d\n", maxHeapUsage);
      printf("Max Stack   = %12d\n", maxStackUsage);
      printf("Max UStack  = %12d\n", maxUStackUsage);
      printf("Max LStack  = %12d\n", maxLStackUsage);
      printf("==========================\n");
  }
  else
      printf("%d\n", stack[0].contents.num);

  if (profiling)
      displayProfTable();

#ifdef ONEBITGC_STUDY1
  if (gcCount)
      fprintf(stderr,
              "One bit reference count could have caught %5.1f%% of the garbage\n",
              100 * sumGc1bitpart / gcCount);
#endif

  return 0;
}
