module main;
   reg clock = 0;
   always #5 clock = ~clock;

   reg a = 0;
   reg b = 0;

   wire sum, carry;

   HalfAdd HalfAdd_inst(clock, 1'b 0, a, b, sum, carry);

   always @(posedge clock) begin
      $display("%05d  %d+%d = %d", $time, a, b, {carry,sum});
      {a,b} <= {a,b} + 1;
   end

   initial #100 $finish;
endmodule
