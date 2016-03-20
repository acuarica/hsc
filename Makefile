

all: Supercompiler
	./$<

app: Append
	./$<

%: %.hs
	ghc -o $@ $< 	
