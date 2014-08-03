OPT=-O -DNDEBUG
#OPT=-g
CFLAGS=-std=c99 $(OPT) -Wall -Werror
EMUOPT=
#EMUOPT=-t

EMU=emu-32-bit

all: emu profemu emu-32-bit #fast-sw-emu

regress: $(EMU)
	$(MAKE) EMU=../emulator/$(EMU) -C ../programs regress-emu

# Poor mans benchmark
bench:
	$(MAKE) $(EMU)
	time $(MAKE) regress

emu: emu.c Makefile
	$(CC) $(CFLAGS) $< -o $@

emu-32-bit: emu-32-bit.c red_atom.h Makefile
	$(CC) $(CFLAGS) $< -o $@

fast-sw-emu: fast-sw-emu.c fast-sw-emu.h Makefile
	$(CC) $(CFLAGS) $< -o $@

profemu: profemu.c
	$(CC) $(CFLAGS) $< -o $@
