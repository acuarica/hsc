# Haskell Supercompiler Project

The Haskell Supercompiler Project explores uses of supercompilation.
Supercompilation is a program transformation technique due to Turchin.
The main use so far of supercompilation was program optimization.
The project aims to support theorem proving on algebraic data structures
as well.

## Modules

### Expr

This module defines the Expr type, 
which is the core language used for all transformations.

### Parser

The parser is a textual representation of Expr.

### Eval

The eval module defines the language semantics of the language.
This module supports two alternatives to reduce expressions,
one is to normal form, and the other to weak head normal form.

### Spliiter

The splitter contains two function, one splits an stucked expression,
and the other, combines, reconstruct the original expression but with the 
splitted parts.

### Supercompile

Supercompiles an expression.

## Testing

For each module, there is cabal target to test it.
All tests are cabal `executable` targets.
The rationale behind this decision, is that `exitcode-stdio-1.0` targets does not show the test output in the console (instead it is saved in a log file).

To execute all module tests, run the following:

```
cabal run test-expr
cabal run test-parser
cabal run test-eval
cabal run test-splitter
cabal run test-supercompiler
```
