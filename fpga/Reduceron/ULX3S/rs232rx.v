// -----------------------------------------------------------------------
//
// A minimalistic UART receiver
//
// ISC License
//
// Copyright (C) 2014 - 2022  Tommy Thorn <tommy-github2@thorn.ws>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
// -----------------------------------------------------------------------

`timescale 1ns/10ps

/*
 * This RS232 receiver only offers a single element buffer, and
 * as we obviously can't offer backpressure it is the resposibility
 * of the receiver to avoid overflow.
 */

module rs232rx
   ( input  wire        clock
   , output reg   [7:0] data = 0
   , output reg         valid = 0
   , input  wire        ready
   , input  wire        serial_in
   , output reg         overflow = 0
   );

   parameter           frequency  = 25_000_000;
   parameter           bps        =     57_600;
   parameter           period     = (frequency + bps/2) / bps;
   // Worst-case period: 300 bps @ 500 MHz = 2 000 000 ~= 2^19
   parameter           TTYCLK_SIGN = 20; // 2^TTYCLK_SIGN > period_max * 2
   parameter           COUNT_SIGN  = 4;

   reg  [TTYCLK_SIGN:0] ttyclk      = 0; // [-4096; 4095]
   reg  [COUNT_SIGN:0]  count       = 0; // [-16; 15]
   reg  [ 7:0]          shift_in    = 0;
   reg                  rxd         = 0;
   reg                  rxd2        = 0;

   /*
    * The theory: look for a negedge, then wait 1.5 bit period to skip
    * start bit and center in first bit.  Keep shifting bits until a full
    * byte is collected.
    *
    *        Start                        Stop
    * q      ~\__ B0 B1 B2 B3 B4 B5 B6 B7 ~~
    * count        8  7  6  5  4  3  2  1
    */
   always @(posedge clock) begin
      if (ready) begin
         valid <= 0;
         overflow <= 0;
      end

      // Get rid of meta stability.
      {rxd2,rxd} <= {rxd,serial_in};

      if (~ttyclk[TTYCLK_SIGN]) begin
         ttyclk <= ttyclk - 1'd1;
      end else if (count) begin
         if (count == 1) begin
            data <= {rxd2, shift_in[7:1]};
            if (valid & !ready)
              overflow <= 1;
            valid <= 1;
         end

         count       <= count - 1'd 1;
         shift_in    <= {rxd2, shift_in[7:1]}; // Shift in from the left
         ttyclk      <= period - 2'd 2;
      end else if (~rxd2) begin
         // Just saw the negedge of the start bit
         ttyclk      <= (3 * period) / 2 - 2'd 2;
         count       <= 8;
      end
   end
endmodule
