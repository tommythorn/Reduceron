SRC=Reduceron.v toplevel.v \
    ../../york-lava/simulation/altsyncram.v \
    ../../york-lava/simulation/lpm_add_sub.v

top.vcd: reduceron-vcd
	./reduceron-vcd

sim: a.out
	@./a.out | grep -v ^WARNING

a.out: $(SRC)
	@for x in *.mif;do grep : < $$x|sed -e "s,^.*:,," -e "s,;,," > $$x.txt;done
	@iverilog -Isim $^

reduceron-vcd: $(SRC)
	@for x in *.mif;do grep : < $$x|sed -e "s,^.*:,," -e "s,;,," > $$x.txt;done
	@iverilog -o $@ -D VCD -Isim $^
