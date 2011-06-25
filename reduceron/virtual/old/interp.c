#include <stdio.h>

//#define HEAP_SIZE 1048576
#define HEAP_SIZE 1162767

#define IntNode 0
#define ApNode 1
#define FunNode 2
#define ArgNode 3
#define PrimNode 8

#define isIntNode(x) (!((x) & 1) && !((x) & 2))
#define isApNode(x) (((x) & 1) && !((x) & 2))
#define isFunNode(x) (!((x) & 1) && ((x) & 2))
#define isArgNode(x) (((x) & 1) && ((x) & 2))

#define isPrim(x) ((x) & 8)

#define isEnd(x) ((x) & 4)

#define markEnd(x, y) (isEnd(x) ? ((y) | 4) : (y))

#define unEnd(x) ((x) & 0xfffffffb)

#define getArg(x) (((unsigned) (x)) >> 3)
#define getAp(x) (((unsigned) (x)) >> 3)

#define mkIntNode(x) (IntNode | ((x) << 3))
#define mkApNode(x) (ApNode | ((x) << 3))
#define mkFunNode(x) (FunNode | ((x) << 4))

#define PrimEq 1
#define PrimNotEq 2
#define PrimLessEq 4
#define PrimAdd 8
#define PrimSub 16

void read_bytecode(char *fn, int *heap, int *hp)
{
  FILE *fp;
  int c0, c1, c2;

  fp = fopen(fn, "rb");

  if (!fp) { printf("Can't find '%s'\n", fn); exit(1); }

  while ((c0 = fgetc(fp)) != EOF) {
    c1 = fgetc(fp);
    c2 = fgetc(fp);
    heap[*hp] = c0 | (c1 << 8) | (c2 << 16);
    (*hp)++;
  }
}

void swap(int *a, int *b)
{
  int tmp;
  tmp = *a;
  *a = *b;
  *b = tmp;
}

void unwind(int *heap, int *hp, int *sp, int addr)
{
  int node;

  (*sp)++;

  do {
    node = heap[addr++];
    heap[--(*sp)] = node;
  } while (! isEnd(node));
}

void unfold(int *heap, int *hp, int *sp, int addr)
{
  int base, node, numArgs, size, spineLen, lastAddr;

  base = *hp;

  node = heap[addr++];

  numArgs = node & 0xf;
  size = (((unsigned) node) >> 4) & 0x3ff;
  spineLen = (((unsigned) node) >> 14);

  //printf("%i %i %i\n", numArgs, size, spineLen);

  while (size > 0) {
    size--;
    node = heap[addr++];
    if (isArgNode(node)) {
      node = markEnd(node, heap[*sp + getArg(node) + 1]);
    }
    else if (isApNode(node)) {
      node = markEnd(node, mkApNode(base + getAp(node) - 1));
    }

    heap[*hp] = node; (*hp)++;
  }

  *sp += numArgs;
  lastAddr = *hp - spineLen;
  *hp = lastAddr;
  unwind(heap, hp, sp, lastAddr);
}


int interp(int *heap, int *hp)
{
  int sp = HEAP_SIZE-2;
  int top;

  heap[sp] = mkFunNode(0);

  for (;;) {
    top = heap[sp];
    if (isIntNode(top)) {
      if (sp == HEAP_SIZE-2)
        break;
      else {
        swap(&heap[sp], &heap[sp+1]);
      }
    }
    else if (isFunNode(top)) {
      if (isPrim(top)) {
        int a, b;
        a = heap[sp+1] >> 3;
        b = heap[sp+2] >> 3;
        switch (((unsigned) top) >> 4) {
          case PrimEq:
            if (a == b) heap[sp+4] = heap[sp+3];
            sp += 4;
            break;
          case PrimNotEq:
            if (a != b) heap[sp+4] = heap[sp+3];
            sp += 4;
            break;
          case PrimLessEq:
            if (a <= b) heap[sp+4] = heap[sp+3];
            sp += 4;
            break;
          case PrimAdd:
            heap[sp+2] = mkIntNode(a+b);
            sp += 2;
            break;
          case PrimSub:
            heap[sp+2] = mkIntNode(a-b);
            sp += 2;
            break;
        }
      }
      else {
        unfold(heap, hp, &sp, ((unsigned) top) >> 4);
      }
    }
    else if (isApNode(top)) {
      unwind(heap, hp, &sp, ((unsigned) top) >> 3);
    }
  }

  return (top >> 3);
}

int main(int argc, char *argv[]) {
  int *heap;
  int hp = 0, i;

  if (argc != 2) { printf("Usage: interp input.red\n"); exit(1); }
  heap = (int *) malloc(sizeof(int) * HEAP_SIZE);
 
  read_bytecode(argv[1], heap, &hp); hp++;

  printf("Result: %i\n", interp(heap, &hp));
  printf("Heap usage: %i\n", hp);
}
