all: Red1 Red2

Red1: Reduceron.lhs
	ghc -O -cpp -DVERSION=1 Reduceron.lhs -o $@

Red2: Reduceron.lhs
	ghc -O -cpp -DVERSION=2 Reduceron.lhs -o $@
