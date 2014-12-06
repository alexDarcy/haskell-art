all: hexVariation.svg diamondTheory.svg

hexVariation.svg: hexVariation
	./hexVariation -o $@ -w 800

diamondTheory.svg: diamondTheory 
	./diamondTheory -o $@ -w 800

hexVariation: hexVariation.lhs
	ghc --make $<

diamondTheory: diamondTheory.lhs
	ghc --make $<
