a.out: HalfAdd.v main.v; iverilog $^ && ./a.out

HalfAdd.v: HalfAdd.hs
	cd ..;runhaskell -i../modules/: HalfAdd/HalfAdd.hs
