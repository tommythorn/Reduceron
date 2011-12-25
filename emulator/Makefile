CFLAGS=-O2 -g
#TEST=Parts.red
#TEST=Cichelli.red
#TEST=KnuthBendix.red
#TEST=CountDown.red
#TEST=MSS.red
TEST=Sudoku.red

all: emu

bigregress: emu
	for x in ../programs/flite/*.red;do echo $$x ; ./emu < $$x;done

regress: emu
	./emu < ../programs/flite/$(TEST)

all: emu profemu emu-32-bit

emu: emu.c Makefile
	$(CC) $(CFLAGS) $< -o $@

emu-32-bit: emu-32-bit.c red_atom.h Makefile
	$(CC) $(CFLAGS) $< -o $@

profemu: profemu.c
	$(CC) $(CFLAGS) $< -o $@
