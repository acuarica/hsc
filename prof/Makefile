
BUILD=../dist/build
RTS=+RTS -s -RTS
FASTPARSER=$(BUILD)/prof-fastparser/prof-fastparser
PARSER= $(BUILD)/prof-parser/prof-parser

all:
	cd .. && cabal build prof-parser prof-fastparser
	$(FASTPARSER) $(RTS)
	$(PARSER) $(RTS)
