module main;
   reg clock = 0;
   always #5 clock = ~clock;

   reg i = 0;
   wire o;

   Loopy Loopy_inst(clock, i, o);

   always @(posedge clock)
      $display("%05d  o %d", $time, o);

   initial begin
      #30 i = 1;
      #60 $finish;
   end
endmodule
