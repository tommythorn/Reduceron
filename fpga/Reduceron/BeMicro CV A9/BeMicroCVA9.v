module BeMicroCVA9
(
    // Clocks
    input   wire                CLK_24MHZ        // General Purpose Clock Input, also
                                                 // used by on-board USB Blaster II
  , input   wire                DDR3_CLK_50MHZ   // DDR3 HMC Clock Input

    // User I/O (LED, push buttons, DIP switch)
  , output  reg     [ 7:0]      LEDn             // Green User LEDs
  , input   wire    [ 1:0]      KEYn             // user push buttons
  , input   wire    [ 3:0]      DIP_SW           // user DIP switch

`ifdef eeprom
    // I2C EEPROM interface
  , inout   wire                EEPROM_SDA       // serial data/address
  , output  wire                EEPROM_SCL       // serial clock
`endif

`ifdef sdcard
    // micro SD card interface
  , output  wire                SDCLK            // SD clock
  , output  wire                SDCMD            // SD command line
  , inout   wire    [ 3:0]      SDD              // SD data
`endif

`ifdef ddr3
    // DDR3 interface
  , input   wire                DDR3_OCT_RZQIN   // external 100 ohm RZQ resistor connected to this pin (B11)
  , output  wire    [12:0]      DDR3_A           // address bus
  , output  wire    [ 2:0]      DDR3_BA          // bank address bus
  , output  wire                DDR3_CASn        // column address strobe
  , output  wire                DDR3_CLK_P       // clock(p) output to DDR3 memory
  , output  wire                DDR3_CLK_N       // clock(n) output to DDR3 memory
  , output  wire                DDR3_CKE         // clock enable
  , output  wire                DDR3_CSn         // chip select
  , output  wire    [ 1:0]      DDR3_DM          // data mask
  , inout   wire    [15:0]      DDR3_DQ          // data bus (15:8 = lane 1; 7:0 = lane 0)
  , inout   wire    [ 1:0]      DDR3_DQS_P       // data strobe(p)
  , inout   wire    [ 1:0]      DDR3_DQS_N       // data strobe(n)
  , output  wire                DDR3_ODT         // on die termination control
  , output  wire                DDR3_RASn        // row address strobe
  , output  wire                DDR3_RESETn      // reset output to DDR3 memory
  , output  wire                DDR3_WEn         // write enable to DDR3 memory
`endif

`ifdef ethernet
    // Ethernet interface
  , input   wire                ENET_RX_CLK      // RGMII RX Clock Output from PHY
  , output  wire                ENET_GTX_CLK     // RGMII TX Ref Clock Input to PHY
  , output  wire                ENET_RSTn        // Reset Input to PHY
  , input   wire                ENET_INTn        // Interrupt Output from PHY
  , output  wire                ENET_TX_EN       // RGMII TX Control Input to PHY
  , input   wire                ENET_RX_DV       // RGMII RX Control Output from PHY
  , output  wire                ENET_MDC         // Management Data Clock Input to PHY
  , inout   wire                ENET_MDIO        // Management Data I/O
  , output  wire    [ 3:0]      ENET_TXD         // RGMII TX Data Input to PHY
  , input   wire    [ 3:0]      ENET_RXD         // RGMII RX Data Output from PHY
`endif
);

   reg         reset = 0;

   wire [17:0] r;
   wire [ 6:0] s;
   wire [13:0] h;
   wire        iowrite;
   wire        ioread;
   wire [14:0] ioaddr, iowd;
   wire        finish;

   reg  [17:0] res = ~0;
   reg         finish_r = 0;
   reg  [12:0] wd = 0;
   reg  [31:0] prescaler;
   reg  [ 8:0] counter;

   reg [ 1:0]  KEYn_r0, KEYn_r;
   always @(posedge clock) KEYn_r0 <= KEYn;
   always @(posedge clock) KEYn_r  <= KEYn_r0;

   always @(posedge clock)
     reset <= KEYn_r == 1;

   always @(posedge CLK_24MHZ)
     if (prescaler == 0) begin
        prescaler <= 6000000 - 1;
        counter <= counter + 1;
     end
     else
       prescaler <= prescaler - 1;

`ifdef not
   always @* LEDn = {~counter,DIP_SW,~KEYn};
`endif

   always @(posedge clock)
     case (KEYn_r)
       3: LEDn <= ~res[10:3];
       2: LEDn <= /* wd */ ~counter; // Random recognizable pattern
       1: LEDn <= ~s;
       0: LEDn <= ~h;
     endcase

   wire clock = DDR3_CLK_50MHZ; // XXX we can almost certainly go faster with a PLL

   // This must match the writeVerilog line in fpga/Main.hs
   Reduceron Reduceron_inst
      (clock,
       reset,
       r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15], r[16], r[17],
       s[0], s[1], s[2], s[3], s[4], s[5], s[6],
       h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8], h[9], h[10], h[11], h[12], h[13],

       ioaddr[0], ioaddr[1], ioaddr[2], ioaddr[3], ioaddr[4], ioaddr[5], ioaddr[6], ioaddr[7], ioaddr[8], ioaddr[9], ioaddr[10], ioaddr[11], ioaddr[12], ioaddr[13], ioaddr[14],
       iowrite, ioread,
       iowd[0], iowd[1], iowd[2], iowd[3], iowd[4], iowd[5], iowd[6], iowd[7], iowd[8], iowd[9], iowd[10], iowd[11], iowd[12], iowd[13], iowd[14],

       finish);

  reg     [  7: 0] jtaguart_tx_data_r;
  reg              jtaguart_tx_data_valid_r = 0;
  wire             jtaguart_tx_data_ready_o;

  wire    [  7: 0] jtaguart_rx_data_o;
  wire             jtaguart_rx_data_valid_o;
  wire             jtaguart_rx_data_ready_i = 1;

  wire             jtaguart_idle_o;

  alt_jtag_atlantic jtag_uart_0_alt_jtag_atlantic
    ( .clk     (clock)
    , .rst_n   (!reset)

    , .r_dat   (jtaguart_tx_data_r)
    , .r_val   (jtaguart_tx_data_valid_r)
    , .r_ena   (jtaguart_tx_data_ready_o)

    , .t_dat   (jtaguart_rx_data_o)
    , .t_dav   (jtaguart_rx_data_ready_i)
    , .t_ena   (jtaguart_rx_data_valid_o)
    , .t_pause (jtaguart_idle_o)
    );

  defparam jtag_uart_0_alt_jtag_atlantic.INSTANCE_ID = 0,
           jtag_uart_0_alt_jtag_atlantic.LOG2_RXFIFO_DEPTH = 6,
           jtag_uart_0_alt_jtag_atlantic.LOG2_TXFIFO_DEPTH = 6,
           jtag_uart_0_alt_jtag_atlantic.SLD_AUTO_INSTANCE_INDEX = "YES";

   reg [35:0] tx_word;
   reg [ 3:0] tx_nibbles = 2;

   reg        old_gc = 0;

   always @(posedge clock) begin
      if (finish) begin
         res <= r;
         finish_r <= 1;
      end

      old_gc <= s[5];

      jtaguart_tx_data_valid_r <= 0;

      if (tx_nibbles == 0) begin
         if (finish_r) begin
            tx_nibbles <= 11;
            tx_word <= res[16:3];
            finish_r <= 0;
         end
/*
         else if (s[5] && !old_gc) begin
            tx_nibbles <= 3;
            tx_word[35:32] <= 9;
         end
*/
      end else if (jtaguart_tx_data_ready_o) begin
         jtaguart_tx_data_valid_r <= 1;

         if (tx_nibbles == 2)
            jtaguart_tx_data_r <= 13;
         else if (tx_nibbles == 1)
            jtaguart_tx_data_r <= 10;
         else
            jtaguart_tx_data_r <= tx_word[35:32] < 10 ? tx_word[35:32] + 48 : tx_word[35:32] + 55;
      end

      if (jtaguart_tx_data_valid_r) begin
         tx_nibbles <= tx_nibbles - 1;
         tx_word <= {tx_word[31:0],4'd0};
      end
   end
endmodule
