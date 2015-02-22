{-# OPTIONS_GHC -fcontext-stack=1024 #-}

import Lava

{-
 Linear Feedback Shift Register example

 What one would typically write as

    reg [32:0]  lfsr = 0;
    always @(posedge clk)
       lfsr <= {lfsr[31:0], ~lfsr[32] ^ lfsr[19]};

 in Verilog we can express in Lava as a chain of 33 flip-flops

    lfsr = take 33 (iterate (delay low) (delay low feedback))

 where lfsr is a list of taps into the chain.  The head of the list is
 the feedback, lfsr !! 31 is the farthest in the chain, etc.

 By inspecting the resulting Verilog we can observe that the resulting
 RTL is physically identical to what the original Verilog source would
 produce.

-}

lfsr = take 33 (iterate (delay 0) (delay 0 feedback))
  where feedback = inv (lfsr !! 32) <#> (lfsr !! 19)

-- The same thing but as vectors instead (top bit is n0, etc)
lfsrv :: Vec N33 Bit
lfsrv = delay 0 (vshr feedback lfsrv)
  where feedback = inv (lfsrv `vat` n32) <#> (lfsrv `vat` n19)

showBits :: [Bit] -> String
showBits bs = [if bitToBool b then '1' else '0' | b <- reverse bs]
showBitsv = showBits . velems

main :: IO ()
main = do
  putStr $ unlines $ map showBits $ drop 990 $ simulateN 1000 lfsr
  putStr $ unlines $ map showBitsv $ drop 990 $ simulateN 1000 lfsrv
  writeVerilog "rtl" (vecn n33 lfsr) (nameWord "out")
  writeVerilog "rtlv" lfsrv (nameWord "out")
