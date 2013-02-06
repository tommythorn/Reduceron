module toplevel
   (
    input         CLOCK_50,              //  50 MHz
    input  [ 3:0] KEY,                   //  Pushbutton[3:0]
    output [ 8:0] LEDG,                  //  LED Green[8:0]
    output [17:0] LEDR                   //  LED Red[17:0]
   );

   wire [15:0] r;
   reg  [15:0] res = ~0;
   wire [ 6:0] s;
   wire [12:0] h;
   wire        iowrite;
   wire [12:0] ioaddr, iowd;
   wire        finish;

   reg [12:0]  wd = 0;

   assign      LEDR       =   (!KEY[3] ? res[15:3] :
                                !KEY[2] ? s         :
                                !KEY[1] ? h         :
                                !KEY[0] ? wd        :
                                'h 5555);

   assign      LEDG       =   {res[2:0],s};

   always @ (posedge CLOCK_50) begin
      if (iowrite)
         wd <= iowd;

      if (finish)
         res <= r;
   end

   Reduceron Reduceron_inst
      (CLOCK_50,
       r[0], r[1], r[2], r[3], r[4], r[5], r[6], r[7],
       r[8], r[9], r[10], r[11], r[12], r[13], r[14], r[15],

       s[0], s[1], s[2], s[3], s[4], s[5], s[6],
       h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8], h[9], h[10], h[11], h[12],
       iowrite,
       ioaddr[0], ioaddr[1], ioaddr[2], ioaddr[3], ioaddr[4], ioaddr[5], ioaddr[6], ioaddr[7], ioaddr[8], ioaddr[9], ioaddr[10], ioaddr[11], ioaddr[12],
       iowd[0], iowd[1], iowd[2], iowd[3], iowd[4], iowd[5], iowd[6], iowd[7], iowd[8], iowd[9], iowd[10], iowd[11], iowd[12],
       finish);
endmodule
