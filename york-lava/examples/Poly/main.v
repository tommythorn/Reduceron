module main;
   reg clock = 0;
   always #5 clock = ~clock;

   wire [7:0] r;
   wire       done;

   Poly Poly_inst
      (clock, r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], done);

   always @(posedge clock)
      $display("%05d  res %d done %d", $time, r, done);

   initial #1500 $finish;
endmodule
