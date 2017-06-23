module Mult(Mult(..), newMult, multiply) where

import Prelude hiding ((.))
import Lava
import Recipe
import Control.Monad

data Mult n = Mult { a, b, result :: Reg n }

newMult :: N n => New (Mult n)
newMult = return Mult `ap` newReg `ap` newReg `ap` newReg

shiftAndAdd :: N n => Mult (S n) -> Recipe
shiftAndAdd s = do
    while (s.b.val =/= 0) $do
       s.a <== low `vshr` (s.a.val)
       s.b <== (s.b.val) `vshl` low
       iff (s.b.val.vhead) $do
           s.result <== s.result.val + s.a.val
       tick

multiply x y s = do s.a <== x; s.b <== y; s.result <== 0; tick; s.shiftAndAdd

example :: Mult N8 -> Recipe
example s = s.multiply 5 25

simExample = simRecipe newMult example result

main =
  do let (s, done) = recipe newMult example (delay high low)
     writeVerilog "Mult"
                  (s.result.val, done)
                  (nameWord "result", name "done")
