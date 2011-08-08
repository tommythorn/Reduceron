module main;
   reg clock = 0;
   always #5 clock = ~clock;

   wire [17:0] r;
   wire [ 6:0] s;
   wire [14:0] h;
   wire        finish;

   Reduceron Reduceron_inst
      (clock,
       r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15], r[16], r[17],
       s[0], s[1], s[2], s[3], s[4], s[5], s[6],
       h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8], h[9], h[10], h[11], h[12], h[13], h[14],
       finish);

   always @(posedge clock) begin
      if (finish)
         $display("%05d  res %d state %x heap %d finish %d", $time, r, s, h, finish);
      if (finish)
         $finish;
   end
endmodule
