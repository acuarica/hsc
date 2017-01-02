
BUILD=../dist/build
RTS=+RTS -s -RTS

all:
	cd .. && cabal build prof-parser prof-fastparser
	$(BUILD)/prof-fastparser/prof-fastparser 1 $(RTS) 2> prof-1k-fastparser.stats
	$(BUILD)/prof-parser/prof-parser         1 $(RTS) 2> prof-1k-parser.stats
	$(BUILD)/prof-fastparser/prof-fastparser 10 $(RTS) 2> prof-10k-fastparser.stats
	$(BUILD)/prof-parser/prof-parser         10 $(RTS) 2> prof-10k-parser.stats
	$(BUILD)/prof-fastparser/prof-fastparser 100 $(RTS) 2> prof-100k-fastparser.stats
	$(BUILD)/prof-parser/prof-parser         100 $(RTS) 2> prof-100k-parser.stats
	$(BUILD)/prof-fastparser/prof-fastparser 1000 $(RTS) 2> prof-1m-fastparser.stats
	$(BUILD)/prof-parser/prof-parser         1000 $(RTS) 2> prof-1m-parser.stats
	$(BUILD)/prof-fastparser/prof-fastparser 10000 $(RTS) 2> prof-10m-fastparser.stats
	$(BUILD)/prof-parser/prof-parser         10000 $(RTS) 2> prof-10m-parser.stats
