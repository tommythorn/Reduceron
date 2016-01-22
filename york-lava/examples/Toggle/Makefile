toggle: Toggle.v main.v
	iverilog -o $@ $^
	./toggle

Toggle.v: Toggle.hs
	cd ..;runhaskell -i../modules/: Toggle/Toggle.hs
