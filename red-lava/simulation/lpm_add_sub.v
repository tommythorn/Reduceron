module lpm_add_sub (result, cout, overflow, add_sub, cin, dataa, datab, clock, clken, aclr);
   parameter lpm_type = "lpm_add_sub";
   parameter lpm_width = 1;
   parameter lpm_direction  = "UNUSED";
   parameter lpm_representation = "UNSIGNED";
   parameter lpm_pipeline = 0;
   parameter lpm_hint = "UNUSED";
   input [lpm_width-1:0] dataa, datab;
   input                 add_sub, cin;
   input                 clock;
   input                 clken;
   input                 aclr;
   output [lpm_width-1:0] result;
   output                 cout, overflow;

// assign                 {cout,result} = add_sub ? dataa - datab - cin : dataa + datab + cin;
   assign                 {cout,result} = (dataa + datab) + cin;
   assign                 overflow = 'h x;
endmodule
