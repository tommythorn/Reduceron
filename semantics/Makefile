all: Red1 Red2 Red3
	./Red1|tail
	./Red2|tail
	./Red3|tail

Red1: Reduceron.lhs
	ghc -O -cpp -DVERSION=1 Reduceron.lhs -o $@

Red2: Reduceron.lhs
	ghc -O -cpp -DVERSION=2 Reduceron.lhs -o $@

Red3: Reduceron.lhs
	ghc -O -cpp -DVERSION=3 Reduceron.lhs -o $@
