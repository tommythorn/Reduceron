module HbcOnly where

space :: Int -> String
space n = if n == 0 then ""
          else ' ':space (n-1)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (b,a,c) -> a
snd3 (_,a,_) = a

thd3 :: (b,c,a) -> a
thd3 (_,_,a) = a

makeDouble :: Integer -> Double -> Int -> Double
makeDouble i f e = (fromIntegral i +f) * (10.0 ^^ e)
