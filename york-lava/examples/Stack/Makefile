a.out: Stack.v main.v
	iverilog ../../simulation/altsyncram.v $^
	./a.out

Stack.v: ../Stack.hs
	cd ..;runhaskell -i../modules/: Stack.hs
