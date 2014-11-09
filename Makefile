hexVariation: hexVariation.o
	./$@ -o test.svg -w 400

%.o: %.hs
	ghc $<
