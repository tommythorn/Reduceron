module Main where
import Lava
import Recipe
import Control.Monad

-- toggle bits


data Toggler = Toggler { output :: Reg N1 }

newToggler :: New Toggler
newToggler = return Toggler `ap` newRegInit 1

toggler :: Toggler -> Recipe
toggler s = While 1 $
               Seq [ s!output <== vmap inv (s!output!val)
                   , Tick
                   ]

main =
  do let (s, done) = recipe newToggler toggler (delay high low)
     writeVerilog "Toggle"
                  (s!output!val, done)
                  (nameWord "output", name "done")
