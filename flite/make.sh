if [ -z "$1" ]
then
	if [ -e "fl.hs" ]
	then
		echo "Compiling whatever was last compiled. ./clean.sh to reset."
		ghc -O2 --make fl
	else
		echo "Compiling fl-pure by default."
		cp -f fl-pure.hs fl.hs
		ghc -O2 --make fl
	fi
else
	echo "Compiling fl-parsec."
	cp -f fl-parsec.hs fl.hs
	ghc -O2 --make fl
fi
