hexVariation: hexVariation.o
	./$@ -o hexVariation.svg -w 400

%.o: %.hs
	ghc $<
