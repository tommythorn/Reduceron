CFLAGS=-O2 -Wall -Werror
#CFLAGS=-g -Wall -Werror
#TEST=Parts.red
#TEST=Cichelli.red
#TEST=KnuthBendix.red
#TEST=CountDown.red
#TEST=MSS.red
TEST=Sudoku.red
EMUOPT=
#EMUOPT=-t

all: emu

bigregress: emu
	for x in ../programs/gold/compiled/*.red;do echo $$x ; ./emu $(EMUOPT) < $$x;done

regress: emu
	./emu $(EMUOPT) < ../programs/gold/compiled/$(TEST)

all: emu profemu emu-32-bit

emu: emu.c Makefile
	$(CC) $(CFLAGS) $< -o $@

emu-32-bit: emu-32-bit.c red_atom.h Makefile
	$(CC) $(CFLAGS) $< -o $@

profemu: profemu.c
	$(CC) $(CFLAGS) $< -o $@
