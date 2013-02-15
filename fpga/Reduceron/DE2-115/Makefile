program: Reduceron.sof
	quartus_pgm Reduceron.cdf

Reduceron.sof: toplevel.v ../Reduceron.v ram_c128.mif
	quartus_map Reduceron
	quartus_fit Reduceron
	quartus_asm Reduceron
