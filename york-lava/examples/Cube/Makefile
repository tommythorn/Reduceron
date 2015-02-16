a.out: main.v Cube.v; iverilog $^ && ./a.out

Cube.v: Cube.hs
	cd ..;runhaskell -i../modules/:Mult: Cube/Cube.hs
