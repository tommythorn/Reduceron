regress:
	$(MAKE) -C programs

hw:
	$(MAKE) -C programs regress-red-verilog-run
