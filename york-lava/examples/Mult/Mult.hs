module Mult(Mult(..), newMult, multiply) where

import Lava
import Recipe
import Control.Monad

data Mult n = Mult { a, b, result :: Reg n }

newMult :: N n => New (Mult n)
newMult = return Mult `ap` newReg `ap` newReg `ap` newReg

shiftAndAdd s =
  While (s!b!val =/= 0) $
    Seq [ s!a <== s!a!val!shr
        , s!b <== s!b!val!shl
        , s!b!val!vhead |>
            s!result <== s!result!val + s!a!val
        , Tick
        ]

shr x = low +> vinit x

shl x = vtail x <+ low

multiply x y s =
  Seq [ s!a <== x, s!b <== y, s!result <== 0, Tick, s!shiftAndAdd ]

example :: Mult N8 -> Recipe
example s = s!multiply 5 25

simExample = simRecipe newMult example result

main =
  do let (s, done) = recipe newMult example (delay high low)
     writeVerilog "Mult"
                  (s!result!val, done)
                  (nameWord "result", name "done")
