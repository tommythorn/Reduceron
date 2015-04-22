
# Reduceron Expanded
# Reduceron Embedded
# Reduceron, The Next Generation
# Reduceron, A New Hope


## WHAT IS REDUCERON?

Reduceron is a high performance FPGA softcore for running lazy functional
programs, complete with hardware garbage collection.  Reduceron has been
implemented on various FPGAs with clock frequency ranging from 60 to 150
MHz depending on the FPGA.  A high degree of parallelism allows Reduceron
to implement graph evaluation very efficiently.

Reduceron is the work of Matthew Naylor, Colin Runciman and Jason Reich,
who have kindly made their work available for others to use.  Please see
http://www.cs.york.ac.uk/fp/reduceron for supporting articles, memos, and
original distribution.


## OK, WHAT'S THIS THEN?

The present is a fork of the original distribution which intends to take
the (York) Reduceron from the research prototype to the point where it
can be useful for embedded projects and more.

The York Reduceron needs the following enhancements to meet our needs:

 0. The heap and program must (for the most parts) be kept in external
    memory, with FPGA block memory used for the stacks and heap and
    program caches.

    This simultaneously enables smaller and less expensive FPGAs to be
    used as well as allows for a much larger heap and larger programs.

 1. Access to memory mapped IO devices (and optionally, RAM).

 2. Richer set of primitives, including multiplication, shifts, logical
    and, or, ...

 3. Support for 32-bit integers - this greatly simplifies interfacing to
    existing IO devices and simplifies various numerical computations.

 4. Stack, update stack, [and case table stack?] should overflow
    into/underflow from external, allowing for orders of magnitude
    larger structures.


While Reduceron technically refers to the FPGA implementation, it is
supported by

 - Flite: the F-lite to Red translator.
 - A Red emulator in C
 - York Lava: Reduceron is a York Lava program, which generate VHDL and
   Verilog
 - Support for Verilog simulation and synthesis for various FPGA dev
   kits.


As much of the history as was available has been gathered and reduceron,
york-lava, and the Flite distribution have been merged into one
repository.


## HOW DO I USE IT?

The was last tested with Haskell Platform 2012.4.0.0 for Mac OS X and
Linux, 64-bit, but I expect it can be made to work on Windows.

Optionally: just run make in the toplevel directory and a large
regression run will start. The Verilog simulation part will take weeks to
finish.

To build:

   make

Or run a specific test suite:

    make -C programs $X

where $X is one of `regress-emu`, `regress-flite-sim`, `regress-flite-comp`, or
`regress-red-verilog-sim`.

To build a hardware version of a given test

    cd fpga; make && flite -r ../programs/$P | ./Red -v

where $P is one of the programs (.hs).  Next, build a Reduceron system
for DE2-115:

    make -C Reduceron/DE2-115

Unfortunately programs can't currently be loaded dynamically but are
baked into the FPGA image.  It's a high priority goal to change that.

## WHERE IS THIS GOING?

Plan:

  1. Port to Verilog and Altera. DONE!

  2. Shrink to fit mid-sized FPGA kits (eg. DE2-115). DONE!

  3. Support load/store to an external bus (the key difficulty is stalling while waiting on the bus).

  4. Use the program memory as a cache, making programs dynamically loadable and dramatically raise the size limits.

In some order:

  - Add basic primitives (`(*)`, `(.&.)`, `(.|.)`, `(.^.)`, `(.<<.)`, `(.>>.)`, ...)
    DONE: `(.&.)`

  - Improve Lava simulation and RTL gen
       - Add a C backend for much faster simulation
       - Use names from the design
       - generate busses where possible
       - Make the design resettable

  - Move the heap [and tospace] to external memory
  - Add a heap cache/newspace memory
  - Implement the emu-32.c representation for the external heap

  - Haskell front-end

Finally: Performance improvements


## OPEN QUESTIONS, with answers from Matthew:

Q1: Currently there doesn't seem an efficient way to handle toplevel
    variable bindings (CAFs).  What did the York team have in mind there
    or does it require an extension?  (Obviously one can treat them all
    other functional arguments, but that would mean a lot of parameters
    to pass around).

A1: "Some mechanism would be needed to construct graphs at a specified
location on the heap at the beginning of program execution.  The
initial (unevaluated) graphs have constant size so can be linked to at
compile time."


Q2: Why does Flite default to 0 for the MAXREGS parameter?  Eg, why is

      redDefaults = CompileToRed 6 4 2 1 0

A2: (Historical reasons it would appear).


Q3: What happend to Memo 24?

A3: "I'd like to say it was our best kept secret, but in reality it
probably got trashed :)"

[![tip for next commit](http://prime4commit.com/projects/273.svg)](http://prime4commit.com/projects/273)
