all: Reduceron1 Reduceron2 Reduceron3 Reduceron4
	./Reduceron1|tail -6
	./Reduceron2|tail -6
	./Reduceron3|tail -6
	./Reduceron4|tail -6

Reduceron%: Reduceron%.hs
	ghc -O -cpp $^ -o $@

Reduceron1.hs: Reduceron.lhs version.lhs
	runhaskell version.lhs 1 Reduceron.lhs > $@

Reduceron2.hs: Reduceron.lhs version.lhs
	runhaskell version.lhs 2 Reduceron.lhs > $@

Reduceron3.hs: Reduceron.lhs version.lhs
	runhaskell version.lhs 3 Reduceron.lhs > $@

Reduceron4.hs: Reduceron.lhs version.lhs
	runhaskell version.lhs 4 Reduceron.lhs > $@
