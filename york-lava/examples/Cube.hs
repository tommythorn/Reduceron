import Lava
import Recipe
import Mult

-- Example illustrating shared procedure calls:
-- compute x*x*x using a single multiplier.

data Cube =
  Cube {
    mul     :: Mult N8
  , mulProc :: Proc
  , arg1    :: Reg N8
  , arg2    :: Reg N8
  }

newCube :: New Cube
newCube =
  do { reg1 <- newReg
     ; reg2 <- newReg
     ; mul <- newMult
     ; mproc <- newProc (mul!multiply (reg1!val) (reg2!val))
     ; return (Cube mul mproc reg1 reg2)
     }

cube :: Word N8 -> Cube -> Recipe
cube x c =
  Seq [
    c!arg1 <== x
  , c!arg2 <== x
  , Tick
  , c!mulProc!call
  , c!arg1 <== c!mul!result!val
  , Tick
  , c!mulProc!call
  ]

fiveCubed :: Cube -> Recipe
fiveCubed c = c!cube 5

simFiveCubed = simRecipe newCube fiveCubed (result . mul)

synFiveCubed =
  do let (s, done) = recipe newCube fiveCubed (delay high low)
     writeVerilog "FiveCubedExample"
                  (s!mul!result!val, done)
                  (nameWord "result", name "done")
