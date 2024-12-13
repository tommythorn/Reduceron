toggle: Toggle.v main.v
	iverilog -o $@ $^
	./toggle

Toggle.v: Toggle.hs
	cd ..;runhaskell -Wno-x-partial -i../modules/: Toggle/Toggle.hs
