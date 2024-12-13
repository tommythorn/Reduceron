avalon: Avalon.v main.v
	iverilog -o $@ $^ ../../simulation/lpm_add_sub.v
	./avalon

Avalon.v: Main.hs
	cd ..;runhaskell -Wno-x-partial -i../modules/: Avalon/Main.hs
