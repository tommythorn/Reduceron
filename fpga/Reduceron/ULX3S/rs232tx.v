// -----------------------------------------------------------------------
//
// A minimalistic UART transceiver
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

module rs232tx
   ( input  wire        clock
   , input  wire  [7:0] data
   , input  wire        valid
   , output wire        ready
   , output wire        serial_out
   );

   parameter           frequency   = 0;
   parameter           bps         = 0;
   parameter           period      = (frequency + bps/2) / bps;
   // Worst-case period: 300 bps @ 500 MHz = 2 000 000 ~= 2^19
   parameter           TTYCLK_SIGN = 20; // 2^TTYCLK_SIGN > period_max * 2
   parameter           COUNT_SIGN  = 4;

   reg  [TTYCLK_SIGN:0] ttyclk      = 0; // [-4096; 4095]
   reg  [8:0]           shift_out   = ~0;
   reg  [COUNT_SIGN:0]  count       = 0; // [  -16;   15]

   assign               serial_out  = shift_out[0];
   assign               ready       = count[COUNT_SIGN] & ttyclk[TTYCLK_SIGN];

   always @(posedge clock)
      if (~ttyclk[TTYCLK_SIGN]) begin
         ttyclk     <= ttyclk - 1'd 1;
      end else if (~count[COUNT_SIGN]) begin
         ttyclk     <= period - 2'd 2;
         count      <= count - 1'd 1;
         shift_out  <= {1'd 1, shift_out[8:1]};
      end else if (valid) begin
         ttyclk     <= period - 2'd 2;
         count      <= 9; // 1 start bit + 8 d + 1 stop - 1 due to SIGN trick
         shift_out  <= {data, 1'd 0};
      end
endmodule
