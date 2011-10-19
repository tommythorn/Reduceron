sim: a.out
	./a.out

a.out: Reduceron.v main.v ../../york-lava/simulation/altsyncram.v ../../york-lava/simulation/lpm_add_sub.v
	for x in *.mif;do grep : < $$x|sed -e "s,^.*:,," -e "s,;,," > $$x.txt;done
	iverilog -Isim $^
