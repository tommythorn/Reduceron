a.out: MultExample.v main.v; iverilog $^ && ./a.out

MultExample.v: ../Mult.hs
	cd ..;runhaskell -i../modules/: Mult.hs
