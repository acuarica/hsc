
HSC=cd .. && cabal run hsc

EXPRS=$(wildcard expr/*.expr) $(wildcard expr/preds/*.expr)
DOTS=$(EXPRS:%=%.dot)
PNGS=$(DOTS:%=%.png)

.PHONY: all expr clean

all: $(PNGS)

expr:
	touch $(EXPRS)

%.dot.png: %.dot
	cat $< | dot -Tpng > $@

%.dot: %
	$(HSC) samples/$<

clean:
	rm $(PNGS)
