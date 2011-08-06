rm -rf "Clean System Files"
rm -f a.out
ls *.icl | xargs -i basename {} .icl | xargs -i rm -f {}
