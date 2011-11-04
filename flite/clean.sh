find . -name "*.hi" | xargs rm -vf {}
find . -name "*.o" | xargs rm -vf {}
find . -name "*~" | xargs rm -vf {}
rm -vf fl
