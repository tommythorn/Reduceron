a.out: main.v Cube.v; iverilog $^ && ./a.out

Cube.v: Cube.hs
	cd ..;runhaskell -Wno-x-partial -i../modules/:Mult: Cube/Cube.hs
