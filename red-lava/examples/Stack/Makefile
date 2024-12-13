a.out: Stack.v main.v
	iverilog ../../simulation/altsyncram.v $^
	./a.out

Stack.v: Stack.hs
	cd ..;runhaskell -Wno-x-partial -i../modules/: Stack/Stack.hs
	@for x in *.mif;do grep : < $$x|sed -e "s,^.*:,," -e "s,;,," > $$x.txt;done
