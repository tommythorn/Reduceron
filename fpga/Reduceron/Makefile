a.out: Reduceron.v main.v altsyncram.v
	for x in *.mif;do grep : < $$x|sed -e "s,^.*:,," -e "s,;,," > $$x.txt;done
	iverilog $^ && ./a.out
