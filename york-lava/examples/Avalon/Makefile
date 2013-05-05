avalon: avalon.v main.v
	iverilog -o $@ $^
	./avalon

avalon.v: Avalon.hs
	cd ..;runhaskell -i../modules/: Avalon/Avalon.hs
