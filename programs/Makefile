# Work around
# "Warning: Installation path .../reduceron/flite/dist
#         not found on the PATH environment variable."
export PATH:=$(PATH):$(PWD)/flite/dist

#WORKLOADS=$(patsubst %.hs,%,$(shell ls *.hs))
# Workloads sorted by time to execute
#
# Sudoku after Parts, but is omitted as it uses "emit" which has no hardware
# support
WORKLOADS=And SmallFib Example Fib Parts CountDown KnuthBendix Adjoxo \
          Cichelli Taut While MSS Braun Clausify Queens OrdList Queens2 \
          PermSort SumPuz Mate2 Mate

BENCHLOADS=Cichelli Braun OrdList Queens2 MSS Queens PermSort SumPuz Mate2 Mate

EMU=../emulator/emu
FLITE=../flite/dist/flite
FLITE_RED=-r6:4:2:1:8 -i1 -s
# Surprisingly there isn't a clear winner among h[0-3] i[0-3] s[0-1],
# but this came out ahead
FLITE_OPTS=-h3 -i1
RED=../fpga/Red
# CC=gcc-8 on macOS, gcc is a fake, use Mac Home Brew to get the real McCoy
CFLAGS=-O2 -DNDEBUG
QUIET=@

regress:
	$(MAKE) regress-emu
	$(MAKE) regress-flite-sim
	$(MAKE) regress-flite-comp
	$(MAKE) regress-red-sim
	$(MAKE) regress-flite-c-comp
	$(MAKE) regress-red-verilog-sim

regress-hw: \
         regress-red-verilog-run

bench: bench-flite-c-comp

$(EMU): ../emulator/emu.c
	$(MAKE) -C ../emulator emu

$(FLITE):
	$(MAKE) -C ../flite
	touch flite.built

.PHONY: $(RED)
$(RED):
	$(MAKE) -C ../fpga


regress-emu: $(patsubst %,%.emu-checked,$(WORKLOADS))

%.emu-checked: gold/compiled/%.red $(EMU)
	$(EMU) $< | diff -u $(patsubst gold/compiled/%.red,gold/run/%.out,$<) - && touch $@

regress-flite-sim: $(patsubst %,%.flite-sim-checked,$(WORKLOADS))

%.flite-sim-checked: %.hs $(FLITE)
	$(FLITE) -v $< | diff -u $(patsubst %.hs,gold/run/%.out,$<) - && touch $@

regress-flite-comp: $(patsubst %,%.flite-comp-checked,$(WORKLOADS))

regress-flite-c-comp: $(patsubst %,%.flite-c-comp-checked,$(WORKLOADS))

%.flite-comp-checked: %.hs $(EMU) $(FLITE)
	$(FLITE) $(FLITE_OPTS) $(FLITE_RED) $< | $(EMU) - | diff -u $(patsubst %.hs,gold/run/%.out,$<) - && touch $@

%.c: %.hs $(FLITE)
	$(FLITE) $(FLITE_OPTS) -c $< > $@

%.exe: %.c $(FLITE)
	$(QUIET)$(CC) $(CFLAGS) -DPRINT_RESULT $< -o $@

%.flite-c-comp-checked: %.exe $(EMU) $(FLITE)
	./$< | diff -u $(patsubst %.exe,gold/run/%.out,$<) - && touch $@

regress-red-sim:
	$(QUIET)echo "regress-red-sim isn't implemented, as simulation in York Lava very quickly runs out of memory."

regress-red-verilog-sim: $(patsubst %,%.red-verilog-sim-checked,$(WORKLOADS))

%.red-verilog-sim-checked: gold/compiled/%.red $(RED)
	$(QUIET)cd verilog-sim;../$(RED) --output-name=$(patsubst gold/compiled/%.red,%,$<) --verilog ../$< > /dev/null
	$(QUIET)time $(MAKE) --no-print-directory -C verilog-sim/$(patsubst gold/compiled/%.red,%,$<) sim | diff -u $(patsubst gold/compiled/%.red,gold/run/%.out,$<) - && touch $@

regress-red-verilog-run: $(patsubst %,%.red-verilog-run-checked,$(WORKLOADS))

%.red-verilog-run-checked: gold/compiled/%.red $(RED)
	cd ../fpga; ./Red -v ../programs/$< && cp Reduceron/*.mif Reduceron/"BeMicro CV A9"
	$(MAKE) -C ../fpga/Reduceron/"BeMicro CV A9"

comma:= ,
empty:=
space:= $(empty) $(empty)

bench-flite-c-comp: $(patsubst %,%.bench-flite-c-comp,$(BENCHLOADS))
	$(QUIET)cat {$(subst $(space),$(comma),$(BENCHLOADS))}.bench-flite-c-comp|sed -e 's/.*m//' -e 's/s//'

%.bench-flite-c-comp: %.exe
	$(QUIET)(time ./$<;time ./$<;time ./$<;time ./$<;time ./$<) 2>&1 | grep ^user | sort | head -1 | tee $@
