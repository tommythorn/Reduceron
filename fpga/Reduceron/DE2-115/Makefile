# why I need sudo here?
program: Reduceron.sof
	sudo quartus_pgm Reduceron.cdf

Reduceron.sof: toplevel.v ../Reduceron.v ram_c129.mif
	quartus_map Reduceron
	quartus_fit Reduceron
	quartus_asm Reduceron
	quartus_sta Reduceron
