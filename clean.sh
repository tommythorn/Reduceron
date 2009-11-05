find | grep "\.hi$" | xargs rm -vf {}
find | grep "\.o$" | xargs rm -vf {}
find | grep "~$" | xargs rm -vf {}
rm -vf fl
