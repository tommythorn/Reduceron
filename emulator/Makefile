OPT_FAST=-O -DNDEBUG
OPT_DEBUG=-g
OPT=$(OPT_FAST)
CFLAGS=-std=c99 $(OPT) -Wall -Werror
EMUOPT=
#EMUOPT=-t

EMU=emu-32-bit

all: emu emu-32-bit #fast-sw-emu

run: $(EMU)
	$(MAKE) EMU=../emulator/$(EMU) -C ../programs regress-emu

# Poor mans benchmark
bench:
	$(MAKE) OPT="$(OPT_FAST)" run
	time $(MAKE) regress

regress:
	$(MAKE) OPT="$(OPT_DEBUG)" run

emu: emu.c Makefile
	$(CC) $(CFLAGS) $< -o $@

emu-32-bit: emu-32-bit.c red_atom.h Makefile
	$(CC) $(CFLAGS) $< -o $@

fast-sw-emu: fast-sw-emu.c fast-sw-emu.h Makefile
	$(CC) $(CFLAGS) $< -o $@
