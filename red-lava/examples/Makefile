subdirs=Avalon Cube HalfAdd Loopy Mult Poly Stack Toggle LFSR

regress-examples: $(patsubst %,%.checked,$(subdirs))

%.checked: %
	$(MAKE) -C $< && touch $@
