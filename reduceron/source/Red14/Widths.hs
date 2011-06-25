module Red14.Widths where

import Lava
import CircLib.Bit
import CircLib.Word

wordSize       :: Int
wordSize       =  18

addr32Width    :: Int
addr32Width    =  15

addr4Width     :: Int
addr4Width     =  12
  
addr12Width    :: Int
addr12Width    =  14

word i         =  i `ofWidth` wordSize

addr4 i        =  i `ofWidth` addr4Width

addr12 i       =  i `ofWidth` addr12Width

addr32 i       =  i `ofWidth` addr32Width

trunc4         =  take addr4Width

trunc12        =  take addr12Width

trunc32        =  take addr32Width
