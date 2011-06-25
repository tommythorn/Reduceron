module Operators where

import Signal
import Generic
import Error

infix  4 <==>
infixr 3 <&>
infixr 2 <|>, ==>
infixr 2 <=>, <#>
infixr 1 |->

----------------------------------------------------------------------
-- Gates

and2 (x, y) = andl [x, y]
or2  (x, y) = orl  [x, y]
xor2 (x, y) = xorl [x, y]

nand2 = inv . and2
nor2  = inv . or2
xnor2 = inv . xor2

equiv (x, y) = xnor2 (x, y)
impl  (x, y) = or2   (inv x, y)

nandl = inv . andl
norl  = inv . orl

plus  (x, y) = plusl [x,y]
sub   (x, y) = plusl [x, neg y]
times (x, y) = timesl [x, y]
imod  (x, y) = modulo x y
idiv  (x, y) = divide x y

----------------------------------------------------------------------
-- Binary Operators

x |->  y = delay  x  y
x <==> y = equal (x, y)

x <&> y = and2  (x, y)
x <|> y = or2   (x, y)
x <#> y = xor2  (x, y)
x <=> y = equiv (x, y)
x ==> y = impl  (x, y)
--x <== y = impl  (y, x)

x %% y     = imod (x, y)
gte (x, y) = gteInt x y
x >>== y   = gte (x, y)

imin (x, y) = ifThenElse (x >>== y) (y, x)
imax (x, y) = ifThenElse (x >>== y) (x, y)

class SignalInt a where
  toSignalInt   :: Signal a   -> Signal Int
  fromSignalInt :: Signal Int -> Signal a

instance SignalInt Int where
  toSignalInt   = id
  fromSignalInt = id

instance SignalInt a => Num (Signal a) where
  x + y    = fromSignalInt $ plus (toSignalInt x, toSignalInt y)
  x - y    = fromSignalInt $ sub (toSignalInt x, toSignalInt y)
  x * y    = fromSignalInt $ times (toSignalInt x, toSignalInt y)
  negate x = fromSignalInt $ neg (toSignalInt x)

  fromInteger = fromSignalInt . int . fromInteger

instance SignalInt a => Fractional (Signal a) where
  x / y = fromSignalInt $ idiv (toSignalInt x, toSignalInt y)

instance SignalInt a => Enum (Signal a) where
  toEnum n = fromSignalInt (int n)
  fromEnum (Signal s) =
    case unsymbol s of
      Int n -> n
      _     -> wrong Error.EnumOnSymbols

instance SignalInt a => Ord (Signal a) where
  min x y = fromSignalInt $ imin (toSignalInt x, toSignalInt y)
  max x y = fromSignalInt $ imax (toSignalInt x, toSignalInt y)



----------------------------------------------------------------------
-- Convert

int2bit n = n <==> (1 :: Signal Int)
bit2int b = ifThenElse b (1 :: Signal Int, 0)

----------------------------------------------------------------------
-- the end.

