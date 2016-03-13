

all: Supercompiler
	./$<

%: %.hs
	ghc -o $@ $< 	
