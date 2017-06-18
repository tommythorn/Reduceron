{- |

A library to export a pipeline Altera Avalon bus master with support
for waitrequest and readdatavalid (Avalon comes in many variants, but
this is a fairly generic version).

The AvalonMaster state just holds the output registers (read, write,
writedata, address) and the inputs (waitrequest, readdatavalid,
readdata).  writeMaster will block until the write is accepted (posted
write), while readMaster will block until the read request is accepted
AND the data is available.  Clients of readMaster are responsible for
sampling av_readdata immediately after readMaster as it will not be
valid in the next cycle).

-}

module Avalon where
import Prelude hiding (Word, (.))
import Lava
import Recipe
import Control.Monad

data AvalonMaster w =
  AvalonMaster {
    av_write         :: Reg N1
  , av_read          :: Reg N1
  , av_writedata     :: Reg w
  , av_address       :: Reg w
  , av_waitrequest   :: Bit
  , av_readdatavalid :: Bit
  , av_readdata      :: Word w
  }

newAvalonMaster :: N w => Bit -> Bit -> Word w -> New (AvalonMaster w)
newAvalonMaster waitrequest readdatavalid readdata = do

  write <- newReg
  read <- newReg
  writedata <- newReg
  address <- newReg

  return $ AvalonMaster {
             av_write = write
           , av_read = read
           , av_writedata = writedata
           , av_address = address
           , av_waitrequest = waitrequest
           , av_readdatavalid = readdatavalid
           , av_readdata = readdata
           }

writeMaster :: N w => Word w -> Word w -> AvalonMaster w -> Recipe
writeMaster addr value s =
  Seq [ s.av_write <== 1
      , s.av_address <== addr
      , s.av_writedata <== value
      , Do Tick (s.av_waitrequest)
      , s.av_write <== 0
      ]

readMaster :: N w => Word w -> AvalonMaster w -> Recipe
readMaster addr s =
  Seq [ s.av_read <== 1
      , s.av_address <== addr
      , Do Tick (s.av_waitrequest)
      , s.av_read <== 0
      , While (inv (s.av_readdatavalid)) Tick
      ]
