
GHC=ghc
EXE=PCSimp

.PHONY: compile, run

compile: $(EXE)

run: compile
	./$(EXE) +RTS -s -RTS

%: %.hs
	$(GHC) --make -o $@ -O2 $<
