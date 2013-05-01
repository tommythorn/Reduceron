module toplevel;
   reg clock = 0;
   always #5 clock = ~clock;

   wire       output1;
   wire       done;
   reg        ready = 0;

   Toggle Toggle_inst(clock, ready, output1, done);

   always @(posedge clock)
      $display("%05d  ready %d output %d done %d", $time, ready, output1, done);

   initial begin
      #41 ready = 1;
      #40 ready = 0;
      #40 ready = 1;

      #100 $finish;
   end
endmodule
