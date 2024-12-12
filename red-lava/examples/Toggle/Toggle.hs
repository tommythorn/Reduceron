module Main where
import Prelude hiding (Word, (.))
import Lava
import Recipe
import Control.Monad

-- toggle bits


data Toggler = Toggler { output :: Reg N1 }

newToggler :: New Toggler
newToggler = return Toggler `ap` newRegInit 1

toggler :: Bit -> Toggler -> Recipe
toggler ready s = while 1 $ do
                     while (inv ready) tick
                     s.output <== vmap inv (s.output.val)
                     tick

main =
  do let (s, done) = recipe newToggler (toggler ready) (delay high low)
     writeVerilog "Toggle"
                  (s.output.val, done)
                  (nameWord "output", name "done")
  where ready = name "ready"
