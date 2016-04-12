module main;
   reg clock = 0;
   always #5 clock = ~clock;

   wire [7:0] r;
   wire       done;

   Cube Cube_inst
      (clock, 1'b 0, r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], done);

   always @(posedge clock)
      $display("%05d  res %d done %d", $time, r, done);

   initial #150 $finish;
endmodule
