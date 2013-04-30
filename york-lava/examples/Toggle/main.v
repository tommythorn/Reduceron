module toplevel;
   reg clock = 0;
   always #5 clock = ~clock;

   wire       output1;
   wire       done;

   Toggle Toggle_inst(clock, output1, done);

   always @(posedge clock)
      $display("%05d  output %d done %d", $time, output1, done);

   initial #100 $finish;
endmodule
