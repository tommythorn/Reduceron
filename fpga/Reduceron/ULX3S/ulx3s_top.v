/* When F1 is depressed (under LEDs, closet to edge connector)
 * generate some 3 Mb/s=300 KB/s traffic on the serial line
 *
 * @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~@ABC...
 *
 * screen /dev/ttyUSB0 3000000
 *
 * Note, without the PLL we can only do 25 MHz/8 = 3.125 Mb/s which is
 * 4.2% over 3 Mb/s and causes corruption.  However we _can_ do 2 Mb/s
 * (with 3.8% error).  Unfortunately there are no supported options
 * between 2 and 3 Mb/s.
 *
 * On macOS, serial ports are hard-limited to 230,400 b/s :(
 * https://apple.stackexchange.com/questions/369767/ftdi-high-speed-serial-on-macos
 */
module top(input        clk_25mhz,
           input [6:0]  btn,
           input        ftdi_txd,
           output       ftdi_rxd,
           output [7:0] led,
           output       wifi_gpio0);

   parameter		in_frequency    = 25_000_000;
   parameter		frequency	= 30_000_000;
   parameter		rate		=  3_000_000;

   wire			clocks_locked;
   wire [3:0]		clocks;
   wire			clock = clocks[0];
   reg			reset = 1;

   reg [7:0]		led_r;
   wire			rs232tx_ready;
   reg [7:0]		rs232tx_data;
   reg [33:0]		ctr = 0;

   // Reduceron connections
   wire [17:0] r;
   wire [ 6:0] s;
   wire [13:0] h;
   wire        iowrite;
   wire        ioread;
   wire [14:0] ioaddr, iowd;
   wire        finish;

   // Tie GPIO0, keep board from rebooting
   assign wifi_gpio0           = 1'b1;
   assign led                  = led_r;

   ecp5pll
     #(
       .in_hz(in_frequency),
       .out0_hz(frequency),
       .out0_tol_hz(frequency/100),
//     .out1_hz(frequency)
       )
   ecp5pll_inst
     (
      .clk_i(clk_25mhz),
      .clk_o(clocks),
      .locked(clocks_locked)
      );

    rs232tx #(frequency, rate) rs232tx_inst
      (clock, rs232tx_data, rs232tx_valid, rs232tx_ready, ftdi_rxd);

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

    always @(posedge clock) begin
       reset <= btn[1] & clocks_locked;

       //rs232tx_valid <= rs232tx_ready & btn[1] & clocks_locked;

       if (rs232tx_ready & rs232tx_valid) begin
	  rs232tx_valid <= 0;
          ctr <= ctr + 1;
       end else if (btn[1] & clocks_locked) begin
	  rs232tx_data <= 8'h40 | ctr[5:0];
	  rs232tx_valid <= 1;
       end

       led_r[7:0] <= ~res[10:3];
    end
endmodule
