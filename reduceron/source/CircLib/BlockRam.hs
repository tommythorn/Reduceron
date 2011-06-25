module CircLib.BlockRam where

import Lava
import Data.Bits
import CircLib.Bit
import CircLib.NumberBase

data BRam18_Inps =
  BRam18_Inps {
    bram18_addr  :: [Bit] -- 10 bit address
  , bram18_datIn :: [Bit] -- 18 bit input
  , bram18_we    :: Bit   -- write enable
  }

bram18 :: [Int] -> BRam18_Inps -> [Bit]
bram18 initial inps = dout
  where
    is = bram18_addr inps
      ++ bram18_datIn inps
      ++ [bram18_we inps]

    res = multi is 18 "RAMB16_S18" options

    dout = map (\i -> multiSel i res) [1..18]

    options = 
         initialConfig18 initial
      ++ unlines [
           "INIT => X\"00000\","
         , "SRVAL => X\"00000\","
         , "WRITE_MODE => \"WRITE_FIRST\""
         ]


initialConfig18 :: [Int] -> String
initialConfig18 as = inits 0 as ++ pinits 0 as
  where
    init n xs = "INIT_" ++ hex 2 n ++ " => X\""
             ++ concatMap (hex 4) xs' ++ "\",\n"
      where
        xs' = reverse $ map (.&. 0xffff) $ xs ++ replicate (16 - length xs) 0

    inits n xs
      | null xs0 = ""
      | otherwise = init n xs0 ++ inits (n+1) xs1
      where
        (xs0, xs1) = splitAt 16 xs

    pinit n xs = "INITP_" ++ hex 2 n ++ " => X\""
             ++ concatMap (hex 1) xs' ++ "\",\n"
      where
        xs' = reverse
            $ pairs (\a b -> (b `shiftL` 2) .|. a)
            $ map (`shiftR` 16)
            $ xs ++ replicate (128 - length xs) 0

    pinits n xs
      | null xs0 = ""
      | otherwise = pinit n xs0 ++ pinits (n+1) xs1
      where
        (xs0, xs1) = splitAt 128 xs

    pairs f []  = []
    pairs f [a] = [f a 0]
    pairs f (a:b:cs) = f a b : pairs f cs

data Dual18_Inps =
  Dual18_Inps {
    dual18_addrA  :: [Bit] -- 10 bit address (port A)
  , dual18_addrB  :: [Bit] -- 10 bit address (port B)
  , dual18_datInA :: [Bit] -- 18 bit input (port A)
  , dual18_datInB :: [Bit] -- 18 bit input (port B)
  , dual18_weA    :: Bit   -- write enable (port A)
  , dual18_weB    :: Bit   -- write enable (port B)
  }

dual18_bram :: [Int] -> Dual18_Inps -> ([Bit], [Bit])
dual18_bram initial inps = (doA, doB)
  where
    is = dual18_addrA inps ++ dual18_addrB inps
      ++ dual18_datInA inps ++ dual18_datInB inps
      ++ [dual18_weA inps] ++ [dual18_weB inps]

    res = multi is 36 "RAMB16_S18_S18" options

    (doA, doB) = splitAt 18 (map (\i -> multiSel i res) [1..36])

    options = 
         initialConfig18 initial
      ++ unlines [
           "INIT_A => X\"00000\","
         , "INIT_B => X\"00000\","
         , "SIM_COLLISION_CHECK => \"ALL\","
         , "SRVAL_A => X\"00000\","
         , "SRVAL_B => X\"00000\","
         , "WRITE_MODE_A => \"WRITE_FIRST\","
         , "WRITE_MODE_B => \"WRITE_FIRST\""
         ]
