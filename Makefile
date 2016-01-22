regress: examples.check regress-most.check
	$(MAKE) -j1 -C programs regress-rtl
	@echo SUCCESS

examples.check:
	$(MAKE) -C york-lava/examples && touch $@

regress-most.check:
	$(MAKE) -C programs regress-most && touch $@

hw:
	$(MAKE) -C programs regress-red-verilog-run
