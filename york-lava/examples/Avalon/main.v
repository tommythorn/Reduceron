module toplevel;
   reg clock = 0;
   always #5 clock = ~clock;

   wire       [3:0] address, writedata;
   wire       write, done;
   reg        waitrequest = 1, waitrequest_ = 1;

   always @(posedge clock) begin
      waitrequest_ <= ~write;
      waitrequest <= waitrequest_ | ~write | ~waitrequest;
   end

   Avalon Avalon_inst(clock,
                      waitrequest,
                      write,
                      address[0],
                      address[1],
                      address[2],
                      address[3],
                      writedata[0],
                      writedata[1],
                      writedata[2],
                      writedata[3],
                      done);

   always @(posedge clock)
      $display("%05d  write %d address %d writedata %d waitrequest %d done %d",
               $time, write, address, writedata, waitrequest, done);

   initial begin
      #100 $finish;
   end
endmodule
