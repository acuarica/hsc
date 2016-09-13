
DOTS=$(wildcard expr/*.dot) $(wildcard hs/*.dot)
PNGS=$(DOTS:%=%.png)

.PHONY: all clean

all: $(PNGS)

%.dot.png: %.dot
	cat $< | dot -Tpng > $@

clean:
	rm $(PNGS)
