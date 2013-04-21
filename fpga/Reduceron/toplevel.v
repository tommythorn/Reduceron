module toplevel;
   reg clock = 0;
   always #5 clock = ~clock;

   wire [17:0] r;
   wire [ 6:0] s;
   wire [13:0] h;
   wire        finish;

   wire        iowrite;
   wire [14:0] ioaddr, iowd;

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

   reg [12:0]  hp = 0;
   reg         gc = 0;

   always @(posedge clock) begin
      if (iowrite)
         $display("%05d  IO Write  [%d] <- %d", $time/10,
                  ioaddr, iowd);
/*
      if (s[5] != gc)
         if (s[5])
            $display("%05d  start  GC", $time/10);
         else
            $display("%05d  finish GC", $time/10);

      if (hp != h)
         $display("%05d  hp = %d", $time/10, hp);
*/

      gc <= s[5];
      hp <= h;

      if (finish)
/*         $display("%05d  res %d (tag %d) state %x heap %d finish %d", $time/10,
                  r / 8, r[2:0], s, h, finish);*/
         $display("%1d", r/8);

      if (finish)
         $finish;
   end
endmodule
