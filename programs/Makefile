#WORKLOADS=$(patsubst %.hs,%,$(shell ls *.hs))
# Workloads sorted by time to execute
#
# Sudoku after Parts, but is omitted as it uses "emit" which has no hardware
# support
WORKLOADS=And Example SmallFib Fib Parts KnuthBendix CountDown Adjoxo \
          Cichelli Taut While Braun MSS Clausify OrdList Queens Queens2 \
          PermSort SumPuz Mate2 Mate

EMU=../emulator/emu
FLITE=../flite/dist/build/flite/flite
FLITE_OPTS=-r6:4:2:1:8 -i1 -s
RED=../fpga/Red

regress: regress-emu \
         regress-flite-sim \
         regress-flite-comp \
         regress-red-sim \
         regress-red-verilog-sim \
         regress-red-verilog-run

$(EMU): ../emulator/emu.c
	$(MAKE) -C ../emulator emu

$(FLITE):
	$(MAKE) -C ../flite

$(RED):
	$(MAKE) -C ../fpga


regress-emu: $(patsubst %,%.emu-checked,$(WORKLOADS))

%.emu-checked: gold/compiled/%.red $(EMU)
	$(EMU) $< | diff -u $(patsubst gold/compiled/%.red,gold/run/%.out,$<) - && touch $@

regress-flite-sim: $(patsubst %,%.flite-sim-checked,$(WORKLOADS))

%.flite-sim-checked: %.hs $(FLITE)
	$(FLITE) -v $< | diff -u $(patsubst %.hs,gold/run/%.out,$<) - && touch $@

regress-flite-comp: $(patsubst %,%.flite-comp-checked,$(WORKLOADS))

%.flite-comp-checked: %.hs $(EMU) $(FLITE)
	$(FLITE) $(FLITE_OPTS) $< | $(EMU) | diff -u $(patsubst %.hs,gold/run/%.out,$<) - && touch $@

regress-red-sim:
	echo Not implemented, as simulation in York Lava very quickly runs out of memory.

.NOTPARALLEL: regress-red-verilog-sim

regress-red-verilog-sim: $(patsubst %,%.red-verilog-sim-checked,$(WORKLOADS))

%.red-verilog-sim-checked: gold/compiled/%.red $(RED)
	cd ../fpga; ./Red -v ../programs/$<
	time $(MAKE) -C ../fpga/Reduceron | diff -u $(patsubst gold/compiled/%.red,gold/run/%.out,$<) - && touch $@

regress-red-verilog-run: $(patsubst %,%.red-verilog-run-checked,$(WORKLOADS))

%.red-verilog-run-checked: gold/compiled/%.red $(RED)
	cd ../fpga; ./Red -v ../programs/$< && cp Reduceron/*.mif Reduceron/DE2-115
	make -C ../fpga/Reduceron/DE2-115
