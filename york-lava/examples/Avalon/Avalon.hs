module Main where
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
  , m_readdata       :: Reg w
  }

newAvalonMaster :: N w => Bit -> Bit -> Word w -> New (AvalonMaster w)
newAvalonMaster waitrequest readdatavalid readdata = do
  write <- newReg
  read <- newReg
  writedata <- newReg
  address <- newReg
  readdata_var <- newReg
  return $ AvalonMaster {
             av_write = write
           , av_read = read
           , av_writedata = writedata
           , av_address = address
           , av_waitrequest = waitrequest
           , av_readdatavalid = readdatavalid
           , av_readdata = readdata
           , m_readdata = readdata_var
           }

writeMaster :: N w => Word w -> Word w -> AvalonMaster w -> Recipe
writeMaster addr value s =
  Seq [ s!av_write <== 1
      , s!av_address <== addr
      , s!av_writedata <== value
      , Do Tick (s!av_waitrequest)
      , s!av_write <== 0
      ]

readMaster :: N w => Word w -> AvalonMaster w -> Recipe
readMaster addr s =
  Seq [ s!av_read <== 1
      , s!av_address <== addr
      , Do Tick (s!av_waitrequest)
      , s!av_read <== 0
      , While (inv (s!av_readdatavalid)) Tick
      , s!m_readdata <== s!av_readdata
      , Tick
      ]

test :: N w => AvalonMaster w -> Recipe
test s = Seq [ writeMaster a1 v1 s
             , Tick, Tick, Tick
             , writeMaster a2 v2 s
             , writeMaster a3 v3 s
             , readMaster  a2 s
             , writeMaster a2 (s!m_readdata!val + 1) s
             ]
  where
  [a1, a2, a3, v1, v2, v3] = map fromIntegral [1, 2, 3, 3, 1, 2]

main =
  do let (s, done) = recipe s0 test (delay high low)
     writeVerilog "Avalon"
                  ( s!av_read!val!vhead
                  , s!av_write!val!vhead
                  , s!av_address!val
                  , s!av_writedata!val
                  , done)
                  (name "read", name "write", nameWord "address", nameWord "writedata",
                  name "done")
   where
   s0 :: New (AvalonMaster N4)
   s0 = newAvalonMaster waitrequest readdatavalid readdata
   waitrequest = name "waitrequest"
   readdata = nameWord "readdata"
   readdatavalid = name "readdatavalid"
