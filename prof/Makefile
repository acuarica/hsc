
BUILD=../dist/build
RTS=+RTS -s -RTS

all:
	cd .. && cabal build prof-parser prof-fastparser
	$(BUILD)/prof-fastparser/prof-fastparser $(RTS)
	$(BUILD)/prof-parser/prof-parser $(RTS)
