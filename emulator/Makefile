CFLAGS=-std=c99 -O2 -Wall -Werror
#CFLAGS=-g -Wall -Werror
#TEST=Parts.red
#TEST=Cichelli.red
#TEST=KnuthBendix.red
#TEST=CountDown.red
#TEST=MSS.red
TEST=Sudoku.red
EMUOPT=
#EMUOPT=-t

EMU=emu-32-bit

all: emu

regress: $(EMU)
	$(MAKE) EMU=../emulator/$(EMU) -C ../programs regress-emu

all: emu profemu emu-32-bit #fast-sw-emu

emu: emu.c Makefile
	$(CC) $(CFLAGS) $< -o $@

emu-32-bit: emu-32-bit.c red_atom.h Makefile
	$(CC) $(CFLAGS) $< -o $@

fast-sw-emu: fast-sw-emu.c fast-sw-emu.h Makefile
	$(CC) $(CFLAGS) $< -o $@

profemu: profemu.c
	$(CC) $(CFLAGS) $< -o $@
