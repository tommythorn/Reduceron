genbmm.hs
---------

Xilinx provides a cool utility called Data2MEM.  Given a bit-file,
this utility can change the initial contents of a specified set of
block RAMs.  This allows us to change the program in the Reduceron's
code memory without having to resynthesise, which would take ages.

Data2MEM needs a "Block RAM Memory Map" (BMM) file which states the
block RAMs to update.  This is what "genbmm.hs" is for - given the
VHDL instance id of code memory, e.g. "c145", it produces the desired
BMM file.  The instance id of code memory can be determined by looking
at the name of the .txt produced the Reduceron generator; for example
if it is called "init_ram_c145.txt" then the instance id is "c145".

Save the output of genbmm.hs to a file, say code.bmm.  Add this
file to the Xilinx ISE project.  After synthesis, code_bd.bmm will be
produced, and the bit file can have its code contents updated using
Data2MEM:

  data2mem -bm code_bd.bmm -bd queens.mem -bt top.bit -ob queens.bit

This tells data2mem to update the block RAMs specified by code.bmm in
the bit file top.bit with the contents of queens.mem to produce a new
bit file called queens.bit.  Files with a .mem extension can be
produced using the "-m" option of the Reduceron generator.
