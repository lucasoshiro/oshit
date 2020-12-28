.PHONY: oshit
oshit:
	ghc -dynamic -O2 Main.hs -o oshit

debug:
	ghc -dynamic Debug.hs -o oshit-debug

.PHONY: clean
clean:
	find . -name '*.hi' -delete
	find . -name '*.o'  -delete
	rm -f oshit


