ls *.hs | grep -v Flite.hs | xargs -i ghc --make -O {}
