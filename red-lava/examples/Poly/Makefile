a.out: Poly.v main.v
	iverilog ../../simulation/altsyncram.v ../../simulation/lpm_add_sub.v $^
	./a.out

Poly.v: Poly.hs
	cd ..;runhaskell -i../modules/:Mult:Stack: Poly/Poly.hs
	@for x in *.mif;do grep : < $$x|sed -e "s,^.*:,," -e "s,;,," > $$x.txt;done
