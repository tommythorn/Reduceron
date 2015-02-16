a.out: Mult.v main.v; iverilog $^ && ./a.out

Mult.v: Mult.hs
	cd ..;runhaskell -i../modules/: Mult/Mult.hs
