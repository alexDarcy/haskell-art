hexVariation: hexVariation.o
	./$@ -o hexVariation.svg -w 800

%.o: %.hs
	ghc $<
