subdirs=Avalon Cube HalfAdd Loopy MultExample Poly Stack Toggle LFSR

regress-examples: $(patsubst %,%.checked,$(subdirs))

%.checked: %
	$(MAKE) -C $<
