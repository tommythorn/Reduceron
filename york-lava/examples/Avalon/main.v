module toplevel;
   reg clock = 0;
   always #5 clock = ~clock;

   wire [3:0] address, writedata;
   wire       read, write, done;
   reg        waitrequest = 1, waitrequest_ = 1, waitrequest__ = 1, readdatavalid = 0;
   wire       rw = read | write;
   reg  [3:0] val[0:3], readdata;

   always @(posedge clock) begin
      waitrequest__ <= ~rw | ~waitrequest;
      waitrequest_ <= waitrequest__;
      waitrequest <= waitrequest_;

      readdatavalid <= read & ~waitrequest;
      if (write & ~waitrequest)
         val[address] <= writedata;
      else
         readdata <= val[address];
   end

   Avalon Avalon_inst(clock,
                      waitrequest,
                      readdatavalid,
                      readdata[0],
                      readdata[1],
                      readdata[2],
                      readdata[3],
                      read,
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

   always @(posedge clock) begin
      if (rw|readdatavalid|done)
         $display("%05d  rd %d wr %d a %d wd %d waitreq %d rdvalid %d rddata %d done %d",
                  $time, read, write, address, writedata, waitrequest,
                  readdatavalid, readdata, done);
      if (write & ~waitrequest)
         $display("       val[%1d] <= %d", address, writedata);
      if (readdatavalid)
         $display("       val[%1d] -> %d", address, readdata);
   end

   initial begin
      #300 $finish;
   end
endmodule
