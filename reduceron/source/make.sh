#!/bin/sh

export GHC=ghc
#export GHC=/grp/haskell/ghc-6.6/bin/i386-unknown-linux/ghc
#export GHC=/local/d0p6/mfn/ghc-6.8.2/bin/ghc

#export YHC_ROOT=/grp/haskell/yhc-reduceron2/

$GHC -fglasgow-exts --make Main.hs -i$YHC_ROOT/src/libraries/core -i$YHC_ROOT/src/libraries/general -i$YHC_ROOT/depends/play -i../Lava2000/Modules:../Lava2000/Modules/Compilers/Ghc: -o reduceron

#$GHC -fhpc -fglasgow-exts --make Main.hs -i$YHC_ROOT/src/libraries/core -i$YHC_ROOT/src/libraries/general -i$YHC_ROOT/depends/play -i../Lava2000/Modules:../Lava2000/Modules/Compilers/Ghc: -o reduceron
