module main;
   reg clock = 1;
   always #5 clock = ~clock;

   wire [7:0] r;
   wire       done;

   Mult Mult_inst
      (clock, 1'b 0, r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7], done);

   always @(posedge clock) begin
      $display("%3d  res %d done %d", $time / 10, r, done);
      if (done)
        $finish;
   end
endmodule
