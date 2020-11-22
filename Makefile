.PHONY: oshit
oshit:
	ghc -dynamic Main.hs -o oshit

.PHONY: clean
clean:
	find . -name '*.hi' -delete
	find . -name '*.o'  -delete
	rm -f oshit


