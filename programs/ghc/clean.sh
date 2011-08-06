rm -f *.hi *.o
ls *.hs | xargs -i basename {} .hs | xargs -i rm -f {}
