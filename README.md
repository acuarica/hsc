
# HSC: Haskell Supercompiler

## Introduction

Pushing the Boundaries of Call-by-Need Supercompilation

The Haskell Supercompiler Project explores uses of supercompilation.
Supercompilation is a program transformation technique due to Turchin.
The main use so far of supercompilation was program optimization.
The project aims to support theorem proving on algebraic data structures
as well.

Supercompilation is a program transformation technique aimed at reduce unnecessary computation when composing functional programs.
Supercompilation can be used for different purposes.
It was mainly used for optimization purposes.
But it has also interesting uses, such as in theorem proving, or logic programming.
But to date, there is no a mainstream supercompiler used for real academic or commercial projects.
In this document, we propose to study how in reality supercompilation is
well-suited for these wide range applications, and what are the short-comings.

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
cabal run test-match
cabal run test-supercompiler
```

### Comprehensive Test of Supercompilation

Correctness of Supercompiler is done using QuickCheck.
Mention problem about using smallcheck related to depth.

## TODOs

* Implement generalization/distillation.
* Check that effectively that supercompiled expression is
  "better" than the input expression.
  What does "better" mean: How do we compare between two equivalent
  expressions for one better.
* Complete Haskell to Core implementation.
* Fix Eval for normal form (Variable Capture)

## Proposal

* What's the research question? (for Supercompilation)
* Write down a proposal to convince Nate that supercompilation works.

## List of estimated publications

* Visualization tool for supercompilation.
  Helping to understand supercompilation.
* Rewrite rules vs. Supercompilation.
* Parser Combinator with Supercompilation: Better together
* Type-checking by Supercompilation.
  Dependent Type-checking by Supercompilation.
  Type inference, and Dependent type inference.
  How is it related with GADTs?
* SuperCheck: A property-prover for Haskell based on supercompilation.
* Int to Peano and back
  Detecting using of algebraic data type for Supercompilation.
