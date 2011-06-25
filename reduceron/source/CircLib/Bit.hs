module CircLib.Bit (Bit, bit) where

import Lava

type Bit  =  Signal Bool

bit       :: Bool -> Bit
bit False =  low
bit True  =  high
