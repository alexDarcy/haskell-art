all: hexVariation diamondTheory test

hexVariation: hexVariation.o
	./$@ -o hexVariation.svg -w 800

diamondTheory: diamondTheory.o
	./$@ -o $@.svg -w 800

test: test.o
	./$@ -o $@.svg -w 800
	
%.o: %.lhs
	ghc $<

%.o: %.hs
	ghc $<
