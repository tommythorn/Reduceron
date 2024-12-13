a.out: HalfAdd.v main.v; iverilog $^ && ./a.out

HalfAdd.v: HalfAdd.hs
	cd ..;runhaskell -Wno-x-partial -i../modules/: HalfAdd/HalfAdd.hs
