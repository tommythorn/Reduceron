import Lava

halfAdd :: Bit -> Bit -> (Bit, Bit)
halfAdd a b = (sum, carry)
  where
    sum   = a <#> b
    carry = a <&> b

main :: IO ()
main = do print $ halfAdd low high
          writeVerilog "HalfAdd"
                       (halfAdd (name "a") (name "b"))
                       (name "sum", name "carry")
