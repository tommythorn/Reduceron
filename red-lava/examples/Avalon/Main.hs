module Main where
import Prelude hiding (Word, (.))
import Lava
import Recipe
import Control.Monad
import Avalon

test :: N w => AvalonMaster w -> Recipe
test s = do writeMaster a1 v1 s
            tick; tick; tick
            writeMaster a2 v2 s
            writeMaster a3 v3 s
            readMaster  a2 s
            writeMaster a2 (s.av_readdata + 1) s
  where
  [a1, a2, a3, v1, v2, v3] = map fromIntegral [1, 2, 3, 3, 1, 2]

main =
  do let (s, done) = recipe s0 test (delay high low)
     writeVerilog "Avalon"
                  ( s.av_read.val.vhead
                  , s.av_write.val.vhead
                  , s.av_address.val
                  , s.av_writedata.val
                  , done)
                  (name "read", name "write", nameWord "address", nameWord "writedata",
                  name "done")
   where
   s0 :: New (AvalonMaster N4)
   s0 = newAvalonMaster waitrequest readdatavalid readdata
   waitrequest = name "waitrequest"
   readdata = nameWord "readdata"
   readdatavalid = name "readdatavalid"
