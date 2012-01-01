{- |

York Lava is library for describing digital circuits.  Descriptions
can be simulated in Hugs or GHC, and converted to VHDL compatible with
XST, the Xilinx Synthesis Tool or Verilog compatible with Quartus II
and more.  It is largely compatible with Chalmers Lava, but omits some
features and offers a few new ones.

New features include:

  * Behavioural description, using "Recipe".

  * Easy addition of new primitive components ('Lava.Bit.makeComponent').

  * Primitives components with multiple outputs.

  * New primitive components, including RAMs and dual-port RAMs
    ('Lava.Prelude.ram' and 'Lava.Prelude.dualRam').
    Can be of any size and width; when synthesised, Xilinx Core
    Generator (.xco) files are produced.

  * Provides an explicit 'Lava.Bit.Netlist' data structure, allowing
    custom backends to be added.

  * Sized vectors ("Lava.Vector").  For example, bit-vectors
    ('Lava.Prelude.Word') are an instance of the /Num/ class.

  * A "Lava.Prelude" of commonly-used circuits.

  * Testing via QuickCheck and SmallCheck.

  * Smallish, self-contained code base.

Limitations include:

  * No model-checking backend; Chalmers Lava has many.

  * Long-running simulations consume memory rather quickly!  Note
    however that you can also simulate circuits using the Xilinx
    simulator, via the VHDL backend.

See @REDUCERON MEMO 23@ - included in the package and available at
<http://www.cs.york.ac.uk/fp/reduceron/> - for a tutorial.

-}

module Lava
  ( module Lava.Bit
  , module Lava.Vector
  , module Lava.Vhdl
  , module Lava.Verilog
  , module Lava.C
  , module Lava.Prelude
  ) where

import Lava.Bit
import Lava.Vector
import Lava.Vhdl
import Lava.Verilog
import Lava.C
import Lava.Prelude
