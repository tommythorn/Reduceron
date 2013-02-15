// -----------------------------------------------------------------------
//
//   Copyright 2004,2006-2008 Tommy Thorn - All Rights Reserved
//
//   This program is free software; you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
//   Bostom MA 02111-1307, USA; either version 2 of the License, or
//   (at your option) any later version; incorporated herein by reference.
//
// -----------------------------------------------------------------------

`timescale 1ns/10ps

module rs232out
   (// Control
    input wire         clock,

    // Serial line
    output wire        serial_out,

    input  wire  [7:0] transmit_data,
    input  wire        we,
    output wire        busy);

   parameter           bps         =     9600;
   parameter           frequency   = 50000000;
`ifndef __ICARUS__
   parameter           period      = (frequency + bps/2) / bps;
`else
   // One of the very few simulation artifacts we have to deal with at the source level.
   parameter           period      = 0;
`endif
   parameter           TTYCLK_SIGN = 16; // 2^TTYCLK_SIGN > period * 2
   parameter           COUNT_SIGN  = 4;

   reg  [TTYCLK_SIGN:0] ttyclk      = 0; // [-4096; 4095]
   wire [31:0]          ttyclk_bit  = period - 2;
   reg  [8:0]           shift_out   = 0;
   reg  [COUNT_SIGN:0]  count       = 0; // [-16; 15]

   assign               serial_out  = shift_out[0];
   assign               busy        = ~count[COUNT_SIGN] | ~ttyclk[TTYCLK_SIGN];

   always @(posedge clock)
      if (~ttyclk[TTYCLK_SIGN]) begin
         ttyclk     <= ttyclk - 1'd1;
      end else if (~count[COUNT_SIGN]) begin
         ttyclk     <= ttyclk_bit[TTYCLK_SIGN:0];
         count      <= count - 1'd1;
         shift_out  <= {1'b1, shift_out[8:1]};
      end else if (we) begin
         ttyclk     <= ttyclk_bit[TTYCLK_SIGN:0];
         count      <= 9; // 1 start bit + 8 data + 1 stop - 1 due to SIGN trick
         shift_out  <= {transmit_data, 1'b0};
      end
endmodule
