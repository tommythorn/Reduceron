/*
  Reduceron Atom and App representation,

  Tommy Thorn, October 2011

  This describes how we achieve two seamingly conflicting goals:
  supporting full 32-bit primitive numbers and fitting heap
  applications in 128-bits.

  The motivation for full 32-bit integers are many, but in particular
  much numerical code assumes power-of-two sized integers.  32-bit
  integers will also simplify interfacing with pre-exiting IO devices.

  The motivation for 128-bit heap cells comes portability and what
  most memories are designed for.  Typically native memory widths are
  16-, 32-, and 64-bit, with most modern memory designed for or even
  requiring burst transfers, which burst must frequently be 4 or 8
  words.  Extending the current Reduceron representation to 32-bit
  integers would imply 32+3 = 35-bit atoms, and thus 5+4*35 = 145-bit
  apps.  While this would trivially fit in 256 bits, it would waste
  memory capacity and bandwidth (less so if bursts can be terminated).

  It's important to stress that this is only concerned with the
  representation in external memory. A realistic implementation would
  not operated directly on packed application as done here, but rather
  pack and unpack them as they enter and exit the cache.

  Fitting atoms in just 33 bit was the primary goal, but the secondary
  goal was to allow as much pointer address space as possible.  Thus,
  some of the complications below comes from minimizing the
  application overhead (the "Head Tag"), for example the elimination
  of the size field and the partially implicit representation of the
  application tag.



  Atom representation

  All atoms are represented as 33-bit values, where the top bit
  indicates whether the rest is an unboxed 32-bit primitive integer or
  a tagged value.  Tagged values have bottom Head Tag (HT) bits
  reserved.

  Effectively we have a multi level tagging scheme (assume HT=5 marked
  with ------):

  - Integers           1NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN integer
  - Tagged Values
    = Pointers         01sppppppppppppppppppppppppp----- shared,heap index
    = Non-pointers
      * Constructors   00000.AAAiiiiiiiiiiiiiiiiiii----- arity,index
      * Primitives     00001sAAAiiiiiiiiiiiiiiiiiii----- arity,swapped,index
      * Arguments      00010s...iiiiiiiiiiiiiiiiiii----- shared,index
      * Registers      00011s...iiiiiiiiiiiiiiiiiii----- shared,index
      * Functions      00100oAAAiiiiiiiiiiiiiiiiiii----- original,arity,index
      * Invalid        00101lr..iiiiiiiiiiiiiiiiiii----- LUT, REGID, index

  Recall, Argument and Register atoms can only occur in
  templates. Invalid is used to mark unused atom slots and implicitly
  represents the size.  Furthermore, for CASE/PRIM, the LUT/RegId is
  encoded in an Invalid atom in the last atom slot with the
  corresponding LUT/REGID bits set.  For simplicity most atoms share
  layout, but that could be change if some, say functions, needs a
  larger address space.


  Heap applications

  While we are primarily concerned with heap allocated apps as opposed
  to apps in templates (which could have a completely different
  representation with hardly any consequence), we sacrifice some
  pointer address range for a simpler representation.

  The HT bits in the first atom are the application tag bits.  For the
  case where the first atom is a primitive number, we recover the HT
  bits from the second atom.  This is possible because it is
  (currently) impossible for a legal heap allocation to have both the
  first and the second atom being a number. (Heap app can't be PRIM
  and can't contain REG and ARG atoms which could further enables us
  to play a few more tricks to save bits here and there).

  These are the heap tag bits:
  - HT_INT0, HT_INT1, HT_INT2, HT_INT3: which atom are numbers
  - HT_NF: application is an irreducible normal form

  Thus, HT is currently 5, however it could be reduced to 3 as
  follows:

  - HT_NF is strictly redundant and can be recovered, but is included
    currently as in the original Reduceron-2 as the payoff is high
    compared to the cost.

  - Only primitive applications can have a number at the head, so it
    may be possible to represent them swapped thus the need for
    NUM_TAG0 disappears.

  Comparing the App tags from the original emulator:

  - The size isn't explicitly represented, but instead unused atoms
    are marked with an illegal value.

  - lut and regId take the place of the last atom (apparently the
    compiler already expects this for LUTs and PRIMs are all currently
    binary).

  - The App tag is implicitly represented by a combination of the GC
    flag and the last atom.

  With an HT of 5, we left with 30-5 = 25 bits for the pointer, enough
  for 32 Mi heap cells, a 512 MiB heap.  The more complicated scheme
  reduces HT to 3 bit, thus leaving 27 bits for the pointer (= 128 Mi
  cells = 2 GiB).
*/

#ifndef _RED_ATOM_H
#define _RED_ATOM_H 1

#include <stdint.h>
#include "red_types.h"

#include <assert.h>

typedef uint64_t Atom;
typedef uint32_t UInt;

#define HT 5

typedef enum { CON, PRI, ARG, REG, FUN, INV  } AtomTag;
typedef enum { ADD, SUB, EQ, NEQ, LEQ, EMIT, EMITINT, SEQ,
               AND, ST32, LD32, LAST_PRIM} Prim;

static inline bool isINT(Atom a)              {return a >> 32;}
static inline Int  getINTValue(Atom a)        {return (Int) a;}
static inline Atom mkINT(Int v)               {
    Atom r = (uint32_t) v | (1ULL << 32);
    assert(getINTValue(r) == v);
    return r;}

static inline bool isPTR(Atom a)              {return (a >> 31) == 1;}
static inline bool getPTRShared(Atom a)       {return (a >> 30) & 1;}
static inline Int  getPTRId(Atom a)           {return ((Int) a << 2) >> (HT + 2);} // Signextend
static inline Atom mkPTR(bool shared, Int id) {
    Atom a = (1U << 31) + (shared << 30) + ((id << HT) & ~(3 << 30));
    assert(isPTR(a));
    assert(getPTRShared(a) == shared);
    assert(getPTRId(a) == id);
    return a;}
static inline Atom setPTRId(Atom a, Int id)   {return mkPTR(getPTRShared(a), id) | (a & ~(-1 << HT));}

static inline UInt atomTag(Atom a)            {return (a >> 28);}
static inline bool atomBool(Atom a)           {return (a >> 27) & 1;}
static inline UInt atomArity(Atom a)          {return (a >> 24) & 7;}
static inline UInt atomIndex(Atom a)          {return (a >> HT) & ~(-1 << (24-HT));}
static inline Atom mkAtom(AtomTag t, bool b, UInt a, UInt i) {
    Atom r = (t << 28) + (b << 27) + (a << 24) + (i << HT);
    assert(atomTag(r) == t);
    assert(atomBool(r) == b);
    assert(atomArity(r) == a);
    assert(atomIndex(r) == i);
    return r;}

static inline bool isFUN(Atom a)              {return atomTag(a) == FUN;}
static inline bool getFUNOriginal(Atom a)     {return atomBool(a);}
static inline UInt getFUNArity(Atom a)        {return atomArity(a);}
static inline UInt getFUNId(Atom a)           {return atomIndex(a);}
static inline Atom mkFUN(bool o, UInt a, UInt i){return mkAtom(FUN,o,a,i);}

static inline bool isARG(Atom a)              {return atomTag(a) == ARG;}
static inline bool getARGShared(Atom a)       {return atomBool(a);}
static inline UInt getARGIndex(Atom a)        {return atomIndex(a);}
static inline Atom mkARG(bool shared, UInt id){return mkAtom(ARG,shared,0,id);}

static inline bool isREG(Atom a)              {return atomTag(a) == REG;}
static inline bool getREGShared(Atom a)       {return atomBool(a);}
static inline UInt getREGIndex(Atom a)        {return atomIndex(a);}
static inline Atom mkREG(bool shared, UInt id){return mkAtom(REG,shared,0,id);}

static inline bool isCON(Atom a)              {return atomTag(a) == CON;}
static inline UInt getCONArity(Atom a)        {return atomArity(a);}
static inline UInt getCONIndex(Atom a)        {return atomIndex(a);}
static inline Atom mkCON(Int arity, UInt id)  {return mkAtom(CON,0,arity,id);}

static inline bool isPRI(Atom a)              {return atomTag(a) == PRI;}
static inline bool getPRISwap(Atom a)         {return atomBool(a);}
static inline UInt getPRIArity(Atom a)        {return atomArity(a);}
static inline Prim getPRIId(Atom a)           {return atomIndex(a);}
static inline Atom togglePRISwap(Atom a)      {return a ^ (1 << 27);}
static inline Atom mkPRI(Int a, bool s, Prim p){return mkAtom(PRI,s,a,p);}

static inline bool isINV(Atom a)              {return atomTag(a) == INV;}
static inline Atom mkINV(void)                {return mkAtom(INV,0,0,0);}

static inline bool isLUT(Atom a)              {return atomTag(a) == INV && atomBool(a);}
static inline UInt getLUTIndex(Atom a)        {return atomIndex(a);}
static inline Atom mkLUT(UInt i)              {return mkAtom(INV,1,0,i);}

static inline bool isPRIM(Atom a)             {return atomTag(a) == INV && atomArity(a);}
static inline UInt getPRIMDest(Atom a)        {return atomIndex(a);}
static inline Atom mkPRIM(UInt rd)            {return mkAtom(INV,0,1,rd);}

static inline UInt getHT(Atom a)              {return a & ~(-1LL << HT);}
static inline Atom setHT(Atom a, UInt ht)     {return (a & (-1LL << HT)) | ht;}

/**** Apps ****/

typedef enum { HT_INT0, HT_INT1, HT_INT2, HT_INT3, HT_NF } HeadTag;

typedef Int Lut;

typedef enum { AP, CASE, PRIM } AppTag;

typedef struct {
    uint32_t atom[APSIZE];
  } App;

static inline bool   isAppCollected(App app) {
    return isINV(app.atom[0]); }

static inline Atom   getAppCollectedAtom(App app) {
    assert(isAppCollected(app));

    return app.atom[1];}

static inline App    mkAppCollected(Atom atom) {
    App app;

    assert(!isINT(atom)); // INT wouldn't make sense here

    app.atom[0] = mkINV();
    app.atom[1] = atom;

    assert(isAppCollected(app));
    assert(getAppCollectedAtom(app) == atom);

    return app;}

static inline AppTag getAppTag(App app) {
    return
        isLUT(app.atom[3]) ? CASE :
        isPRIM(app.atom[3]) ? PRIM :
        AP;}

static inline UInt   getAppSize(App app) {
    assert(!isAppCollected(app));
    return 1 + !isINV(app.atom[1]) + !isINV(app.atom[2]) + !isINV(app.atom[3]);}

static inline Bool   getAppNF(App app) {
    assert(!isAppCollected(app));
    return (getHT(app.atom[0]) >> HT_NF) & 1;}

static inline Lut    getAppLUT(App app) {
    assert(!isAppCollected(app));
    return getLUTIndex(app.atom[3]);}

static inline UInt   getAppRegId(App app) {
    assert(!isAppCollected(app));
    return getPRIMDest(app.atom[3]);}

static inline Atom   getAppAtom(App app, int i) {
    Atom a = app.atom[i];

    assert(!isAppCollected(app));

    /* Recover the integer tag from the head tag */
    a |= (Atom) ((app.atom[0] >> i) & 1) << 32;

    if (i == 0 && isINT(a))
        /* Recover the HT bits from the next atom (which can't also be an integer) */
        a = setHT(a, getHT(app.atom[1]));

    return a;}

#ifndef NDEBUG
static bool atomEq(Atom a, Atom b) {
    return isINT(a) ? a == b : ((a & -1 << HT) == (b & -1 << HT)); }
#endif

static inline App    mkApp(AppTag tag, Int size, bool nf, Int info, Atom *atom) {
    App app;
    int i;
    UInt ht = 0;

    assert(size >= 1);

    /* The core App constraint allowing the packed representation */
    assert(!(size >= 2 && isINT(atom[0]) && isINT(atom[1])));

    for (i = 0; i < size; ++i)
        app.atom[i] = atom[i];
    for (; i < APSIZE; ++i)
        app.atom[i] = mkINV();

    if (isINT(atom[0])) {
        /* We need the HT field so save off those bits in the next atom */
        app.atom[1] = setHT(app.atom[1], getHT(atom[0]));
        ht |= 1 << HT_INT0;
    }

    if (size > 1 && isINT(atom[1]))
        ht |= 1 << HT_INT1;

    if (size > 2 && isINT(atom[2]))
        ht |= 1 << HT_INT2;

    if (size > 3 && isINT(atom[3]))
        ht |= 1 << HT_INT3;

    switch (tag) {
    case CASE: app.atom[3] = mkLUT(info); assert(size < 4); break;
    case PRIM: app.atom[3] = mkPRIM(info); assert(size < 4); break;
    case AP:   ht |= nf << HT_NF; break;
    default: assert(0);
    }

    app.atom[0] = setHT(app.atom[0], ht);

    assert(getAppSize(app) == size);
    assert(getAppTag(app) == tag);
    assert(atomEq(getAppAtom(app, 0), atom[0]));

    if (size > 1)
        assert(atomEq(getAppAtom(app, 1), atom[1]));
    if (size > 2)
        assert(atomEq(getAppAtom(app, 1), atom[1]));
    if (size > 3)
        assert(atomEq(getAppAtom(app, 1), atom[1]));

    return app;}

/* Accelerated access to App atoms */
static inline void shareAppAtom(App *app, int i) {
    assert(!isAppCollected(*app));
    app->atom[i] |= 1 << 30;
}

#endif
