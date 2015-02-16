a.out: Loopy.v main.v; iverilog $^ && ./a.out

Loopy.v: Loopy.hs
	cd ..;runhaskell -i../modules/: Loopy/Loopy.hs
