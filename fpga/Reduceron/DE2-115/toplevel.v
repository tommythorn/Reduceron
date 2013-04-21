module toplevel
   (
    //////// CLOCK //////////
    input         CLOCK_50,              //  50 MHz
    //////// RS232 //////////
    output        UART_CTS,
    input         UART_RTS,
    input         UART_RXD,
    output        UART_TXD,
    //////// KEY //////////
    input  [ 3:0] KEY,                   //  Pushbutton[3:0]
    //////// LED //////////
    output [ 8:0] LEDG,                  //  LED Green[8:0]
    output [17:0] LEDR                   //  LED Red[17:0]
   );

   wire [17:0] r;
   wire [ 6:0] s;
   wire [13:0] h;
   reg  [17:0] res = ~0;
   wire        iowrite;
   wire [14:0] ioaddr, iowd;
   wire        finish;
   reg         finish_r = 0;

   reg [12:0]  wd = 0;

   assign      LEDR       =   (!KEY[3] ? res >> 3  :
                               !KEY[2] ? s         :
                               !KEY[1] ? h         :
                               !KEY[0] ? wd        :
                               'h 5555);

   assign      LEDG       =   {res[2:0],s};

   wire        clock = CLOCK_50;

   // This must match the writeVerilog line in fpga/Main.hs
   Reduceron Reduceron_inst
      (clock,
       r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15], r[16], r[17],
       s[0], s[1], s[2], s[3], s[4], s[5], s[6],
       h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8], h[9], h[10], h[11], h[12], h[13],

       ioaddr[0], ioaddr[1], ioaddr[2], ioaddr[3], ioaddr[4], ioaddr[5], ioaddr[6], ioaddr[7], ioaddr[8], ioaddr[9], ioaddr[10], ioaddr[11], ioaddr[12], ioaddr[13], ioaddr[14],
       iowrite, ioread,
       iowd[0], iowd[1], iowd[2], iowd[3], iowd[4], iowd[5], iowd[6], iowd[7], iowd[8], iowd[9], iowd[10], iowd[11], iowd[12], iowd[13], iowd[14],

       finish);

   reg [ 7:0] rs232out_d = 65;
   reg        rs232out_we = 0;
   wire       rs232out_busy, rs232out_tx;

   assign     UART_TXD = rs232out_tx;

   reg [35:0] tx_word;
   reg [ 3:0] tx_nibbles = 2;

   reg        old_gc = 0;

   always @(posedge CLOCK_50) begin
      if (iowrite)
         wd <= iowd;

      if (finish) begin
         res <= r;
         finish_r <= 1;
      end

      old_gc <= s[5];

      rs232out_we <= 0;

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
      end
      else begin
         rs232out_we <= 1;

         if (tx_nibbles == 2)
            rs232out_d <= 13;
         else if (tx_nibbles == 1)
            rs232out_d <= 10;
         else
            rs232out_d <= tx_word[35:32] < 10 ? tx_word[35:32] + 48 : tx_word[35:32] + 55;

         if (rs232out_we && !rs232out_busy) begin
            tx_nibbles <= tx_nibbles - 1;
            tx_word <= {tx_word[31:0],4'd0};
         end
      end
   end

   rs232out rs232out_inst
      (.clock        (CLOCK_50),
       .serial_out   (rs232out_tx),
       .transmit_data(rs232out_d),
       .we           (rs232out_we),
       .busy         (rs232out_busy));

   defparam  rs232out_inst.frequency = 50_000_000,
             rs232out_inst.bps       =    115_200;
endmodule
