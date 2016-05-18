
module Main where

import Test.HUnit

import Expr (app, appVars, freeVars)
import Parser (parseExpr)

main :: IO Counts
main = runTestTT $ test $
  map testApp [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y")
  ] ++
  map testAppVars [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y")
  ] ++
  map testFreeVars [
    ("x", ["x"]),
    ("f x", ["f", "x"]),
    ("f (g x) (h x y)", ["f", "g", "x", "h", "y"]),
    ("let inc={n->S n} in inc m", ["m"]),
    ("let inc={n->S n} in inc n", ["n"]),
    ("let inc={n->S m} in inc n", ["n", "m"]),
    ("let inc={n->(let s=Succ in s n)} in inc m", ["m"]),
    ("let inc={n->(let s=Succ in s n)} in inc n", ["n"]),
    ("let inc={n->s (let s=A in s m)} in inc m", ["m", "s"])
  ]
  where
    testApp (f, args, e) = "app" ~: f ~:
      app (parseExpr f) (map parseExpr args) ~=? parseExpr e
    testAppVars (f, args, e) = "appVars" ~: f ~:
      appVars (parseExpr f) args ~=? parseExpr e
    testFreeVars (a, e) = "freeVars" ~: a ~: (freeVars . parseExpr) a ~=? e
