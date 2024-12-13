a.out: Mult.v main.v; iverilog $^ ../../simulation/lpm_add_sub.v && ./a.out

Mult.v: Mult.hs
	cd ..;runhaskell -Wno-x-partial -i../modules/: Mult/Mult.hs
