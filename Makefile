# set to your decired level of parallism or nothing at all
#J=
J=-j5

regress: examples.check regress.check
	@echo SUCCESS

examples.check:
	$(MAKE) $(J) -C red-lava/examples && touch $@

regress.check:
	$(MAKE) J=$(J) -C programs regress && touch $@

hw:
	$(MAKE) -C programs regress-red-verilog-run
