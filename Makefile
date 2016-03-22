

all: Supercompiler
	./$<

app: Append
	./$<

%: %.hs
	ghc -o $@ $<

rs:
	rustc test.rs && ./test
