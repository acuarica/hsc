
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

### Match

Defines whether two expressions match.
Also defines unification, homeomorphic embedding, and generalization.

### Supercompiler

Supercompiles an expression.
The splitter contains two function, one splits an stucked expression,
and the other, combines, reconstruct the original expression but with the
splitted parts.

## Testing

For each module, there is cabal target to test it.
All tests are cabal `executable` targets.
The rationale behind this decision, is that `exitcode-stdio-1.0` targets does not show the test output in the console (instead it is saved in a log file).

To execute all module tests, run the following:

```
cabal run test-expr
cabal run test-parser
cabal run test-eval
cabal run test-match
cabal run test-supercompiler
```

### Comprehensive Test of Supercompilation

Correctness of Supercompiler is done using QuickCheck.
Mention problem about using smallcheck related to depth.

## TODOs

### Dependent-type related

* Implement generalization/distillation.
  With this, we'll have a minimal working Supercompiler to experiment with.
*

### Misc

* Check that effectively that supercompiled expression is
  "better" than the input expression.
  What does "better" mean: How do we compare between two equivalent
  expressions for one better.
* Complete Haskell to Core implementation.
* Fix Eval for normal form (Variable Capture)

## Proposal

* What's the research question? (for Supercompilation)
* Language with dependent types based on supercompilation.

## List of estimated publications

* Find a first publication topic for next week.
  Draft of the research question that I want to address.
  Supercompilation Modulo Theories.

1. Visualization tool for supercompilation.
  Helping to understand supercompilation.
  CC'17
2. Parser Combinator with Supercompilation: Better together
  PLDI'17
3. Proposal
  ICSE'17 (DS)
4. Rewrite rules vs. Supercompilation.
  ICPC'17
5. Understanding Rewrite Rules in Hackage.
  MSR'17
6. Int to Peano and back
  Detecting using of algebraic data type for Supercompilation.
  ECOOP'17
7. Type-checking by Supercompilation.
  Dependent Type-checking by Supercompilation.
  Type inference, and Dependent type inference.
  How is it related with GADTs?
  Creating a new type system based on Supercompilation.
  Background on dependent-type checking.
  What flavor of dependent-type do we want?
  Error reporting in type-checking.
  Contracts for JavaScript checked by Supercompilation.
  Comparing Supercompilation with Z3.
  Does supercompilation enable you to prove more things than e.g, Z3?
  Using Z3 in a Supercompiler.
  Stratego, rewrite rules.
  Find a reason to why vouch my supercompiler instead of others.
    There is a special reason to use mine rather others.
  ICLP'17
8. SuperCheck: A property-prover for Haskell based on supercompilation.
  ?

dependent-type language with supercompilation:
Advantage : we dont need totality.
Only provide theorem, supercompilation provides proof?

Where dependent-types fail?
What's the expressive power of supercompilation? What can they prove? Does it rely on totality?
