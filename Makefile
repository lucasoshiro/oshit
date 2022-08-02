.PHONY: oshit
oshit:
	ghc -dynamic -O2 -isrc src/Main.hs -o oshit

debug:
	ghc -dynamic -isrc src/Debug.hs -o oshit-debug

.PHONY: clean TAGS tags
clean:
	find src -name '*.hi' -delete
	find src -name '*.o'  -delete
	rm -f oshit
	rm -f TAGS tags

TAGS:
	hasktags src

tags:
	hasktags src

