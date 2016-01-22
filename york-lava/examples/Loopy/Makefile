a.out: Loopy.v main.v; iverilog $^ ../../simulation/lpm_add_sub.v && ./a.out

Loopy.v: Loopy.hs
	cd ..;runhaskell -i../modules/: Loopy/Loopy.hs
