ls *.icl | grep -v Flite | xargs -i basename {} .icl | xargs -i clm {} -o {}
