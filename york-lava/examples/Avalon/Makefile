avalon: avalon.v main.v
	iverilog -o $@ $^
	./avalon

avalon.v: Main.hs
	cd ..;runhaskell -i../modules/: Avalon/Main.hs
