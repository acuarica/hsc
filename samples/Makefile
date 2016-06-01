
DOTS=$(wildcard *.dot)
PNGS=$(DOTS:%=%.png)

.PHONY: all clean

all: $(PNGS)

%.dot.png: %.dot
	cat $< | dot -Tpng > $@

clean:
	rm $(PNGS)

