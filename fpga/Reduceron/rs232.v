`timescale 1ns/10ps
module rs232(input  wire        clk,
             input  wire        rst,

             // Master connections
             input  wire `REQ   rs232_req,
             output wire `RES   rs232_res,

             input  wire        rs232in_attention,
             input  wire [7:0]  rs232in_data,

             input  wire        rs232out_busy,
             output wire        rs232out_w,
             output wire [7:0]  rs232out_d);

   parameter debug = 1;

`ifdef __ICARUS__
   parameter inputtext  = "input.txt";
   integer   file, ch = 32, rs232in_pending = 0;
`endif

   reg [31:0] tsc = 0;  // A free running counter....
   reg [ 7:0] rs232in_cnt = 0;

   wire [31:0] addr = rs232_req`A;
   reg [31:0] rd_data = 0;
   assign     rs232_res`RD = rd_data;
   assign     rs232_res`HOLD = 0;

   always @(posedge clk)
     if (rst) begin
        rd_data     <= 0;
        tsc         <= 0;
        rs232in_cnt <= 0;
     end else begin
`ifdef __ICARUS__
        if (rs232out_w)
           $display("*** serial output '%c' ***", rs232out_d);
`endif

        rd_data <= 0;
        tsc <= tsc + 1;
`ifndef __ICARUS__
        if (rs232in_attention)
          rs232in_cnt <= rs232in_cnt + 1'h1;
`endif

        if (rs232_req`R) begin
`ifdef __ICARUS__
           case (addr[3:2])
           0: rd_data <= 0;
           1: begin
                $display("*** serial input '%c' ***", ch);
                rd_data <= {24'h0,ch}; //  4
                rs232in_pending = 0;
              end
           2: begin
              if (rs232in_pending > 1) begin
                 rs232in_pending = rs232in_pending - 1;
                 if (rs232in_pending == 1) begin
                    rs232in_cnt = rs232in_cnt + 1;
                 end
              end
              rd_data <= {24'h0,rs232in_cnt};  //  8
              end
           3: rd_data <= tsc;                  // 12
           endcase

           if (!rs232in_pending) begin
              ch = $fgetc(file);
              if (ch != -1)
                 rs232in_pending = 3; // Minimum 2
           end
`else
           case (addr[3:2])
           0: rd_data <= {31'h0,rs232out_busy};//  0
           1: rd_data <= {24'h0,rs232in_data}; //  4
           2: rd_data <= {24'h0,rs232in_cnt};  //  8
           3: rd_data <= tsc;                  // 12
           endcase
`endif
        end
     end

   // wire rs232en = (peri_ctrl_req`A & 'hFFF0) == 'h0000;
   wire [31:0] rs232_writedata = rs232_req`WD;
   assign rs232out_d    = rs232_writedata[7:0];
   assign rs232out_w    = rs232_req`W & addr[3:0] == 0;

`ifdef __ICARUS__
   initial begin
      file = $fopen(inputtext, "r");
      $display("Opening of %s resulted in %d", inputtext, file);
   end
`endif
endmodule
