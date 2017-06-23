create_clock -name CLK_24MHZ -period 41.666 [get_ports {CLK_24MHZ}]
create_clock -name DDR3_CLK_50MHZ -period 20 [get_ports {DDR3_CLK_50MHZ}]

derive_pll_clocks
derive_clock_uncertainty
