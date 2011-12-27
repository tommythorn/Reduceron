WORKLOADS=$(patsubst %.hs,%,$(shell ls *.hs))

EMU=../emulator/emu
FLITE=../flite/dist/build/Flite/flite
RED=../fpga/Red

regress: regress-emu \
         regress-flite-sim \
         regress-flite-comp \
         regress-red-sim \
         regress-red-verilog-sim \
         regress-red-verilog-run

$(EMU): ../emulator/emu.c
	$(MAKE) -C ../emulator emu

flite.built:
	cd ../flite; cabal configure
	cd ../flite; cabal build
	touch $@

$(RED):
	$(MAKE) -C ../fpga


regress-emu: $(patsubst %,%.emu-checked,$(WORKLOADS))

%.emu-checked: gold/compiled/%.red $(EMU)
	$(EMU) < $< | diff -u $(patsubst gold/compiled/%.red,gold/run/%.out,$<) - && touch $@

regress-flite-sim: $(patsubst %,%.flite-sim-checked,$(WORKLOADS))

%.flite-sim-checked: %.hs flite.built
	$(FLITE) -v $< | diff -u $(patsubst %.hs,gold/run/%.out,$<) - && touch $@

regress-flite-comp: $(patsubst %,%.flite-comp-checked,$(WORKLOADS))

%.flite-comp-checked: %.hs $(EMU) $(FLITE)
	$(FLITE) -r6:4:2:1:8 -i1 $< | $(EMU) | diff -u $(patsubst %.hs,gold/run/%.out,$<) - && touch $@

regress-red-sim:
	echo Not implemented, as simulation in York Lava very quickly runs out of memory.

.NOTPARALLEL: regress-red-verilog-sim

regress-red-verilog-sim: $(patsubst %,%.red-verilog-sim-checked,$(WORKLOADS))

%.red-verilog-sim-checked: gold/compiled/%.red $(RED)
	cd ../fpga; ./Red -v ../programs/$<
	time $(MAKE) -C ../fpga/Reduceron | diff -u $(patsubst gold/compiled/%.red,gold/run/%.out,$<) - && touch $@

regress-red-verilog-run:
	echo Not implemented YET. It will be difficult to make very general.
