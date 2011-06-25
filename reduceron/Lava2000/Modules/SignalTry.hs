
import Ref

data Signal a
  = Value a
  | Symbol (Ref (Symbol a))

data Symbol a
  = forall x y . Function String ([x] -> [y] -> a) [Signal x] [Signal y]
  | Delay (Signal a) (Signal a)
  | Variable String

and2 :: (Signal Bool, Signal Bool) -> Signal Bool
and2 (x,y) = Symbol (ref (Function "and" (\xs _ -> and xs) [x,y] []))
