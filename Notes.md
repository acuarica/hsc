# Notes

- performance testing random haskell
- invertible functions? generate instance. random generator.
- cost function, which one to use? estimate. quickcheck to estimate cost.
- optimizations in the fix point rewrite rule.
  Use dynamic programming to implement it.
why map f.g xs is better than map f map g xs?
  map f xs ++ map f ys bettter/worst than map f (xs++ys)
- bake all optimizations into the rules?


## Supercompilation

http://c2.com/cgi/wiki?SuperCompiler

Generalization of partial evaluation.

Performance Haskell, cost measure function.

CTFE (Compile Time Function Evaluation) as part of Supercompilation?
As in Rust?

Another uses for Supercompilation?
Only for optimization?
What if we use a supercompiler with a functional language with no garbage collection?
Would that make sense?

* Supercompile a version of quicksort to more efficient one?

