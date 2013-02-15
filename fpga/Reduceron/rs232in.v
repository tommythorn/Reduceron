// -----------------------------------------------------------------------
//
//   Copyright 2004,2007-2008 Tommy Thorn - All Rights Reserved
//
//   This program is free software; you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, Inc., 53 Temple Place Ste 330,
//   Bostom MA 02111-1307, USA; either version 2 of the License, or
//   (at your option) any later version; incorporated herein by reference.
//
// -----------------------------------------------------------------------

`timescale 1ns/10ps

module rs232in
   (// Control
    input  wire        clock,

    // Serial line
    input  wire        serial_in,
    output reg         attention = 0,
    output reg   [7:0] received_data = 0);

   parameter           bps        =     57_600;
   parameter           frequency  = 25_000_000;
   parameter           period     = (frequency + bps/2) / bps;

   reg  [16:0] ttyclk       = 0;
   wire [31:0] ttyclk_bit   = period - 2;
   wire [31:0] ttyclk_start = (3 * period) / 2 - 2;
   reg  [ 7:0] shift_in     = 0;
   reg  [ 4:0] count        = 0;

   reg         rxd          = 0;
   reg         rxd2         = 0;

   /*
    * The theory: look for a negedge, then wait 1.5 bit period to skip
    * start bit and center in first bit.  Keep shifting bits until a full
    * byte is collected.
    *
    *        Start                        Stop
    * data   ~\__ B0 B1 B2 B3 B4 B5 B6 B7 ~~
    * count        8  7  6  5  4  3  2  1
    */
   always @(posedge clock) begin
      attention <= 0;

      // Get rid of meta stability.
      {rxd2,rxd} <= {rxd,serial_in};

      if (~ttyclk[16]) begin
         ttyclk <= ttyclk - 1'd1;
      end else if (count) begin
         if (count == 1) begin
            received_data <= {rxd2, shift_in[7:1]};
            attention     <= 1;
         end

         count       <= count - 1'd1;
         shift_in    <= {rxd2, shift_in[7:1]}; // Shift in from the left
         ttyclk      <= ttyclk_bit[16:0];
      end else if (~rxd2) begin
         // Just saw the negedge of the start bit
         ttyclk      <= ttyclk_start[16:0];
         count       <= 8;
      end
   end
endmodule
