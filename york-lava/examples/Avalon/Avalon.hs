module Main where
import Lava
import Recipe
import Control.Monad

data WriteMaster =
  WriteMaster {
    av_write       :: Sig N1
  , av_writedata   :: Reg N4
  , av_address     :: Reg N4
  , av_waitrequest :: Bit
  }

newWriteMaster :: Bit -> New WriteMaster
newWriteMaster waitrequest = do
  write <- newSig
  writedata <- newReg
  address <- newReg
  return $ WriteMaster {
             av_write = write
           , av_writedata = writedata
           , av_address = address
           , av_waitrequest = waitrequest
           }

writeMaster :: Int -> Int -> WriteMaster -> Recipe
writeMaster addr value s = Do writeit waitrequest
  where
  waitrequest = name "waitrequest"
  writeit = Seq
             [ s!av_write <== 1
             , s!av_address <== fromIntegral addr
             , s!av_writedata <== fromIntegral value
             , Tick]

test :: WriteMaster -> Recipe
test s = Seq [ writeMaster 1 3 s
             , writeMaster 2 1 s
             , writeMaster 3 2 s
             ]
main =
  do let (s, done) = recipe (newWriteMaster (name "waitrequest")) test (delay high low)
     writeVerilog "Avalon"
                  ( s!av_write!val!vhead
                  , s!av_address!val
                  , s!av_writedata!val
                  , done)
                  (name "write", nameWord "address", nameWord "writedata", name "done")
