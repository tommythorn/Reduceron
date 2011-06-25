#include <stdio.h>

#define STACK_SIZE 1024
#define HEAP_SIZE 32767
#define CODE_SIZE 1024

#define IntNode 0
#define ApNode 1
#define FunNode 2
#define ArgNode 3
#define PrimNode 8

#define isIntNode(x) (!((x) & 1) && !((x) & 2))
#define isApNode(x) (((x) & 1) && !((x) & 2))
#define isFunNode(x) (!((x) & 1) && ((x) & 2))
#define isArgNode(x) (((x) & 1) && ((x) & 2))
#define isGcNode(x) (((x) & 1) && ((x) & 2))

#define isPrim(x) ((x) & 8)

#define isEnd(x) ((x) & 4)

#define markEnd(x, y) (isEnd(x) ? ((y) | 4) : (y))

#define unEnd(x) ((x) & 0xfffffffb)

#define getArg(x) (((unsigned) (x)) >> 3)
#define getAp(x) (((unsigned) (x)) >> 3)

#define gcToAp(x) ((x) & 0xfffffffd)

#define mkIntNode(x) (IntNode | ((x) << 3))
#define mkIntEndNode(x) (IntNode | ((x) << 3) | 4)
#define mkApNode(x) (ApNode | ((x) << 3))
#define mkFunNode(x) (FunNode | ((x) << 4))
#define mkApEndNode(x) (ApNode | ((x) << 3) | 4)
#define mkGcNode(x) (ArgNode | ((x) << 3))

#define PrimEq 1
#define PrimNotEq 2
#define PrimLessEq 4
#define PrimAdd 8
#define PrimSub 16

#define trueAddr (FunNode | (2 << 4) | 4)
#define falseAddr (FunNode | (4 << 4) | 4)

int *heap;
int *heap2;
int *stack;
int *astack;
int *code;
int hp, sp, hp2;
int count = 0;

int copyAp(int src)
{
  int p, addr, node;

  p = src;
  addr = hp2;

  node = heap[p];
 
  //while (isApNode(node) && isEnd(node)) {
  //  printf("here\n");
  //  node = heap[getAp(node)];
  //  heap[src] = mkGcNode(addr);
  //}

  if (isGcNode(node)) {
    return getAp(node);
  }
  else {
    do {
      node = heap[p];
      heap2[hp2++] = node;
      p++;
    } while (! isEnd(node));
    heap[src] = mkGcNode(addr);
    return addr;
  }
}

void fix()
{
  int p, node, res;

  p = 0;

  while (p < hp2) {
    node = heap2[p];
    if (isApNode(node)) {
      res = copyAp(getAp(node));
      heap2[p] = markEnd(node, mkApNode(res));
    }
    p++;
  }
}

inline void collect()
{
  int p = STACK_SIZE-1;
  int a, res;
  int *tmp;

  hp2 = 0;
 
  while (p >= sp) {
    a = stack[p];
    if (isApNode(a)) {
      res = copyAp(getAp(a));
      stack[p] = mkApNode(res);
    }
    p--;
  }

  fix();

  p = STACK_SIZE-1;
  while (p >= sp) {
    a = astack[p];
    if (a) {
      a = heap[a >> 1];
      if (isGcNode(a)) {
        astack[p] = (getAp(a) << 1) | 1;
      }
      else
        astack[p] = 0;
    }
    p--;
  }

  hp = hp2;
  tmp = heap;
  heap = heap2;
  heap2 = tmp;
}

void read_bytecode(char *fn)
{
  FILE *fp;
  int c0, c1, c2;
  int p = 0;

  fp = fopen(fn, "rb");

  if (!fp) { printf("Can't find '%s'\n", fn); exit(1); }

  while ((c0 = fgetc(fp)) != EOF) {
    c1 = fgetc(fp);
    c2 = fgetc(fp);
    code[p] = c0 | (c1 << 8) | (c2 << 16);
    p++;
  }
}

inline void unwind(int addr)
{
  int node, top, root, x = 0;

  root = addr;
  top = sp;
  sp++;

  do {
    node = heap[addr++];
    sp--;
    stack[sp] = node;
    astack[sp] = 0;
    x++;
    count++;
  } while (! isEnd(node));
  //printf("UNWIND: %i\n", x);

  astack[top] = (root << 1) | 1;
}


inline void unfold(int addr)
{
  int base, node, numArgs, size, spineLen, lastAddr, root;

  base = hp;

  node = code[addr++];

  numArgs = node & 0xf;
  size = (((unsigned) node) >> 4) & 0x3ff;
  spineLen = (((unsigned) node) >> 14);

  //count += (int) (size/8);
  count += size;
  //printf("UNFOLD: %i\n", size);

  while (size > 0) {
    size--;
    node = code[addr++];
    if (isArgNode(node)) {
      node = markEnd(node, stack[sp + getArg(node) + 1]);
    }
    else if (isApNode(node)) {
      node = markEnd(node, mkApNode(base + getAp(node) - 1));
    }

    heap[hp] = node; hp++;
  }

  sp += numArgs;
  lastAddr = hp - spineLen;

  root = astack[sp];
  if (root)
    heap[root>>1] = mkApEndNode(lastAddr);

  unwind(lastAddr);
}

inline int interp()
{
  sp = STACK_SIZE-1;
  int top, root;

  stack[sp] = mkFunNode(0);

  for (;;) {
    if (hp >= HEAP_SIZE - 100) {
      collect();
      //printf("Collected %i\n", hp);
    }
    top = stack[sp];
    if (isIntNode(top)) {
      if (sp == STACK_SIZE-1)
        break;
      else {
        int tmp;
        tmp = stack[sp]; stack[sp] = stack[sp+1]; stack[sp+1] = tmp;
      }
    }
    else if (isFunNode(top)) {
      if (isPrim(top)) {
        int a, b;
        a = stack[sp+1] >> 3;
        b = stack[sp+2] >> 3;
        root = astack[sp+2];
        switch (((unsigned) top) >> 4) {
          case PrimEq:
            if (a == b) stack[sp+4] = stack[sp+3];
            if (root) heap[root>>1] = a == b ? trueAddr : falseAddr;
            sp += 4;
            break;
          case PrimNotEq:
            if (a != b) stack[sp+4] = stack[sp+3];
            if (root) heap[root>>1] = a != b ? trueAddr : falseAddr;
            sp += 4;
            break;
          case PrimLessEq:
            if (a <= b) stack[sp+4] = stack[sp+3];
            if (root) heap[root>>1] = a <= b ? trueAddr : falseAddr;
            sp += 4;
            break;
          case PrimAdd:
            if (root) heap[root>>1] = mkIntEndNode(a+b);
            stack[sp+2] = mkIntNode(a+b);
            sp += 2;
            break;
          case PrimSub:
            if (root) heap[root>>1] = mkIntEndNode(a-b);
            stack[sp+2] = mkIntNode(a-b);
            sp += 2;
            break;
        }
      }
      else {
        unfold(((unsigned) top) >> 4);
      }
    }
    else { //if (isApNode(top)) {
      unwind(((unsigned) top) >> 3);
    }
  }

  return (top >> 3);
}

int main(int argc, char *argv[])
{
  int i;

  hp = 0;

  if (argc != 2) { printf("Usage: interp input.red\n"); exit(1); }
  heap = (int *) malloc(sizeof(int) * HEAP_SIZE);
  heap2 = (int *) malloc(sizeof(int) * HEAP_SIZE);
  stack = (int *) malloc(sizeof(int) * STACK_SIZE);
  astack = (int *) malloc(sizeof(int) * STACK_SIZE);
  code = (int *) malloc(sizeof(int) * CODE_SIZE);
 
  read_bytecode(argv[1]);

  printf("Result: %i\n", interp());
  printf("Heap usage: %i\n", hp);
  printf("Count: %i\n", count);
}
