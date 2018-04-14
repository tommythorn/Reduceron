regress: examples.check regress.check
	@echo SUCCESS

examples.check:
	$(MAKE) -C red-lava/examples && touch $@

regress.check:
	$(MAKE) -C programs regress && touch $@

hw:
	$(MAKE) -C programs regress-red-verilog-run
