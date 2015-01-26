O=-O

all: Reduceron1 Reduceron2 Reduceron3 Reduceron4 Reduceron5 Reduceron6
	./Reduceron1|tail -6
	./Reduceron2|tail -6
	./Reduceron3|tail -6
	./Reduceron4|tail -6
	./Reduceron5|tail -6
	./Reduceron6|tail -8

Reduceron%: Reduceron%.hs
	ghc $(O) -cpp -optP-Wno-invalid-pp-token $^ -o $@

Reduceron1.hs: Reduceron.lhs variant.lhs
	runhaskell variant.lhs 1 Reduceron.lhs > $@

Reduceron2.hs: Reduceron.lhs variant.lhs
	runhaskell variant.lhs 2 Reduceron.lhs > $@

Reduceron3.hs: Reduceron.lhs variant.lhs
	runhaskell variant.lhs 3 Reduceron.lhs > $@

Reduceron4.hs: Reduceron.lhs variant.lhs
	runhaskell variant.lhs 4 Reduceron.lhs > $@

Reduceron5.hs: Reduceron.lhs variant.lhs
	runhaskell variant.lhs 5 Reduceron.lhs > $@

Reduceron6.hs: Reduceron.lhs variant.lhs
	runhaskell variant.lhs 6 Reduceron.lhs > $@
