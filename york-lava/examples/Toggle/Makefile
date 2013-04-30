toggle: toggle.v main.v
	iverilog -o $@ $^
	./toggle

toggle.v: Toggle.hs
	cd ..;runhaskell -i../modules/: Toggle/Toggle.hs
