.PHONY: oshit
oshit:
	ghc -dynamic Main.hs -o oshit

.PHONY: clean
clean:
	rm -f **.o **.hi oshit

