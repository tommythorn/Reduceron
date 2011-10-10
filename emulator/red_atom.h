#ifndef _RED_ATOM_H
#define _RED_ATOM_H 1

#include <stdint.h>
#include "red_types.h"

#ifdef assert
#undef assert
#endif
#define assert(x) (void) 0

/*
  This describes how we achieve two seamingly mutually exclusive
  goals: supporting full 32-bit primitive numbers and fitting heap
  applications in 128-bits.

  Atom representation

  All atoms are represented as 33-bit values, where the top bit
  indicates whether the rest is an unboxed 32-bit primitive integer or
  a tagged value.  Tagged values have bottom HeadTag (HT) bits
  reserved.

  Effectively we have a multi level tagging scheme (assume HT=7 marked
  with -------):

  - Integers            1NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN integer
  - Tagged Values
    = Pointers          01sppppppppppppppppppppppp------- shared,heap index
    = Non-pointers
      * Constructors    00000.AAAiiiiiiiiiiiiiiiii------- arity,index
      * Primitives      00001sAAAiiiiiiiiiiiiiiiii------- arity,swapped,index
      * Arguments       00010s...iiiiiiiiiiiiiiiii------- shared,index
      * Registers       00011s...iiiiiiiiiiiiiiiii------- shared,index
      * Functions       00100oAAAiiiiiiiiiiiiiiiii------- original,arity,index
      * Invalid         00101lr..iiiiiiiiiiiiiiiii------- LUT, REGID, index

  Recall, Argument and Register atoms can only occur in
  templates. Invalid is used to mark unused atom slots and implicitly
  represents the size.  Furthermore, for CASE/PRIM, the LUT/RegId is
  encoded in an Invalid atom in the last atom slot with the
  corresponding LUT/REGID bits set.  For simplicity most atoms share
  layout, but that could be change if some, say functions, needs a
  larger address space.

  Heap applications

  While we are primarily concerned with heap allocated apps as opposed
  to apps in templates, which could have a completely different
  representation with hardly any consequence, we sacrifice pointer
  address range for a simpler representation.

  The HT bits in the first atom are the application tag bits.  For the
  case where the first atom is a primitive number, we recover the HT
  bits from the second atom.  This is possible because it is
  (currently) impossible for a legal heap allocation to have both the
  first and the second atom being a number. (Heap app can't be PRIM
  and can't contain REG and ARG atoms which could further enables us
  to play a few more tricks to save bits here and there).

  These are the heap tag bits:
  - HT_INT0, HT_INT1, HT_INT2, HT_INT3: which atom are numbers
  - HT_GC: COLLECTED or {AP, CASE, PRIM}
  - HT_NF: application is an irreducible normal form

  Thus, HT is currently 6, however it could be reduced to 4 as
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

  With an HT of 6, we left with 30 - 6 = 24 bits for the pointer,
  enough for 16 Mi heap cells, a 256 MiB heap.  The more complicated
  scheme reduces HT to 4 bit, thus leaving 26 bits for the pointer (=
  64 Mi cells = 1 GiB).

  This tagging scheme comes from the 128-bit constraint of external
  memory.  If a cache is used, heap application can be unpacked as
  they move from external memory into the cache (and vise versa).
*/

typedef uint64_t Atom;
typedef uint32_t UInt;

#define HT 6

typedef enum { CON, PRI, ARG, REG, FUN, INV  } AtomTag;
typedef enum { ADD, SUB, EQ, NEQ, LEQ, EMIT, EMITINT, SEQ } Prim;

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

typedef enum { HT_INT0, HT_INT1, HT_INT2, HT_INT3, HT_GC, HT_NF } HeadTag;

typedef Int Lut;

typedef enum { AP, CASE, PRIM, COLLECTED } AppTag;

typedef struct {
    uint32_t atoms_[APSIZE];
  } App;

static inline AppTag getAppTag(App a) {
    return
        ((getHT(a.atoms_[0]) >> HT_GC) & 1) ? COLLECTED :
        isLUT(a.atoms_[3]) ? CASE :
        isPRIM(a.atoms_[3]) ? PRIM :
        AP;}

static inline UInt   getAppSize(App a){
    return 1 + !isINV(a.atoms_[1]) + !isINV(a.atoms_[2]) + !isINV(a.atoms_[3]);}
static inline Bool   getAppNF(App a)  {return (getHT(a.atoms_[0]) >> HT_NF) & 1;}
static inline Lut    getAppLUT(App a) {return getLUTIndex(a.atoms_[3]);}
static inline UInt   getAppRegId(App a){return getPRIMDest(a.atoms_[3]);}
static inline Atom   getAppAtom_(App app, int i){
    Atom a = (uint32_t) app.atoms_[i];

    /* This depends on HT_INT3,..,HT_INT0 being in the bottom of the
       first word */
    a |= (Atom) ((app.atoms_[0] >> i) & 1) << 32;

    if (i || !isINT(a))
        return a;
    else
        return setHT(a, getHT(app.atoms_[1]));
}

/* This version of getAppAtom() is just to normalize the HT field */
static inline Atom   getAppAtom(App app, int i) {
    Atom a = getAppAtom_(app, i);
    if (isINT(a))
        return a;
    else
        return setHT(a, 0);
}

static inline App    mkApp(AppTag tag, Int size, bool nf, Int info, Atom *atoms) {
    App app;
    int i;
    UInt ht = 0;

    assert(size >= 1);

    if (isINT(atoms[0])) {
        if (size >= 2 && /*getINTValue(atoms[0]) != 0 && */ !isPRI(atoms[1]))
            /* The INT 0 case came up from a Flite "bug" that I've
               fixed in both Flite and pre-existing binaries */
            printf("An app with an integer at the head, not followed by a primitive\n");
    }

    for (i = 0; i < size; ++i)
        app.atoms_[i] = atoms[i];
    for (; i < APSIZE; ++i)
        app.atoms_[i] = mkINV();

    if (isINT(atoms[0])) {
        UInt x = getHT(atoms[0]);
        app.atoms_[1] = setHT(app.atoms_[1], x);
        assert(getHT(app.atoms_[1]) == x);
        ht |= 1 << HT_INT0;
    }

    if (size > 1 && isINT(atoms[1]))
        ht |= 1 << HT_INT1;

    if (size > 2 && isINT(atoms[2]))
        ht |= 1 << HT_INT2;

    if (size > 3 && isINT(atoms[3]))
        ht |= 1 << HT_INT3;

    switch (tag) {
    case CASE: app.atoms_[3] = mkLUT(info); assert(size < 4); break;
    case PRIM: app.atoms_[3] = mkPRIM(info); assert(size < 4); break;
    case AP:   ht |= nf << HT_NF; break;
    case COLLECTED: ht |= 1 << HT_GC; break;
    }

    app.atoms_[0] = setHT(app.atoms_[0], ht);

    assert(getAppSize(app) == size);
    assert(getAppTag(app) == tag);
    assert(getAppAtom(app, 0) == atoms[0]);
    if (size > 1)
        assert(getAppAtom(app, 1) == atoms[1]);
    if (size > 2)
        assert(getAppAtom(app, 2) == atoms[2]);
    if (size > 3)
        assert(getAppAtom(app, 3) == atoms[3]);

    return app;}

/* Accelerated access to App atoms */
static inline void shareAppAtom(App *a, int i) {
    a->atoms_[i] |= 1 << 30;
}

#endif
