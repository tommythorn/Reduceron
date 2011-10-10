CFLAGS=-O2 -g
#TEST=Parts.red
#TEST=Cichelli.red
#TEST=KnuthBendix.red
#TEST=CountDown.red
TEST=MSS.red

bigregress: myemu
	for x in ../programs/flite/*.red;do echo $$x ; ./myemu < $$x;done

regress: myemu
	./myemu < ../programs/flite/$(TEST)

all: emu profemu myemu

emu: emu.c Makefile
	$(CC) $(CFLAGS) $< -o $@

myemu: myemu.c red_atom.h Makefile
	$(CC) $(CFLAGS) $< -o $@

profemu: profemu.c
	$(CC) $(CFLAGS) $< -o $@
