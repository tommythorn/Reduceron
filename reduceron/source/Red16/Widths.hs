module Red16.Widths where

import Lava
import CircLib.Bit
import CircLib.Word

wordSize       :: Int
wordSize       =  18

addrWidth      :: Int
addrWidth      =  15

stkAddrWidth   :: Int
stkAddrWidth   =  12

combAddrWidth  :: Int
combAddrWidth  =  12

word i         =  i `ofWidth` wordSize

address i      =  i `ofWidth` addrWidth
