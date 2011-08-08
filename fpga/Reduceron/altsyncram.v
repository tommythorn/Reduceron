module altsyncram
   #(parameter address_reg_b = "CLOCK0",
     parameter clock_enable_input_a = "BYPASS",
     parameter clock_enable_input_b = "BYPASS",
     parameter clock_enable_output_a = "BYPASS",
     parameter clock_enable_output_b = "BYPASS",
     parameter init_file = "",
     parameter indata_reg_b = "CLOCK0",
     parameter lpm_type = "altsyncram",
     parameter numwords_a = 4096,
     parameter numwords_b = 4096,
     parameter operation_mode = "BIDIR_DUAL_PORT",
     parameter outdata_aclr_a = "NONE",
     parameter outdata_aclr_b = "NONE",
     parameter outdata_reg_a = "UNREGISTERED",
     parameter outdata_reg_b = "UNREGISTERED",
     parameter power_up_uninitialized = "FALSE",
     parameter read_during_write_mode_mixed_ports = "DONT_CARE",
     parameter read_during_write_mode_port_a = "NEW_DATA_NO_NBE_READ",
     parameter read_during_write_mode_port_b = "NEW_DATA_NO_NBE_READ",
     parameter widthad_a = 12,
     parameter widthad_b = 12,
     parameter width_a = 28,
     parameter width_b = 28,
     parameter width_byteena_a = 1,
     parameter width_byteena_b = 1,
     parameter wrcontrol_wraddress_reg_b = "CLOCK0")
      (input clock0,

       input [widthad_a-1:0] address_a,
       input wren_a,
       input [width_a-1:0] data_a,
       output [width_a-1:0] q_a,

       input [widthad_b-1:0] address_b,
       input wren_b,
       input [width_b-1:0] data_b,
       output [width_b-1:0] q_b,

       input aclr0,
       input aclr1,
       input addressstall_a,
       input addressstall_b,
       input byteena_a,
       input byteena_b,
       input clock1,
       input clocken0,
       input clocken1,
       input clocken2,
       input clocken3,
       input eccstatus,
       input rden_a,
       input rden_b);

   // XXX I assume width_a == width_b && widthad_a == widthad_b
   // XXX I assume byteenables are always fully on
   reg [width_a-1:0] memory[(1 << widthad_a) - 1: 0];
   reg [widthad_a-1:0] address_a_, address_b_;

   assign q_a = memory[address_a_];
   assign q_b = memory[address_b_];

   // XXX I'm not sure this is the best model of the behaviour
   always @(posedge clock0) begin
      if (wren_a)
         memory[address_a] <= data_a;

      if (wren_b)
         memory[address_b] <= data_b;

      address_a_ <= address_a;
      address_b_ <= address_b;
   end

   initial
      if (init_file != "")
         $readmemh({init_file,".txt"}, memory);
endmodule
