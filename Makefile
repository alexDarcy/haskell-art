hexagon: hexagon.o
	./hexagon -o test.svg -w 400

%.o: %.hs
	ghc $<
