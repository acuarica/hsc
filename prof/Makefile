
GHC=ghc
EXE=PCSimp


all:
	../dist/build/prof-parser/prof-parser +RTS -s -RTS
	../dist/build/prof-fastparser/prof-fastparser +RTS -s -RTS

.PHONY: compile, run

compile: $(EXE)

run: compile
	./$(EXE) +RTS -s -RTS

%: %.hs
	$(GHC) --make -o $@ -O2 $<
