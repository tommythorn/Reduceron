create_clock -name CLK_24MHZ -period 41.666 [get_ports {CLK_24MHZ}]

derive_pll_clocks
derive_clock_uncertainty
