
module Main where

import Test.HUnit (Counts, runTestTT, test, (~:), (~?=))

import Expr (Expr(Var), app, appVars, subst, freeVars)
import Parser (parseExpr)

main :: IO Counts
main = runTestTT $ test $
  map (\(f, args, e) -> "app" ~: f ~: app (parseExpr f) (map parseExpr args) ~?= parseExpr e)
  [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y")
  ] ++
  map (\(f, args, e) -> "appVars" ~: f ~: appVars (parseExpr f) args ~?= parseExpr e) [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y")
  ] ++
  map (\(a,s,e)-> "subst" ~: a ~: subst s (parseExpr a) ~?= parseExpr e)
  [
    ("{n->m}", ("m", Var "$0"), "{n->$0}"),
    ("{n->n}", ("n", Var "$0"), "{n->n}"),
    ("let n=A in m", ("m", Var "$0"), "let n=A in $0"),
    ("let n=A in n", ("n", Var "$0"), "let n=A in n"),
    ("case n of Z->Z;S m->l;", ("n", Var "$0"), "case $0 of Z->Z;S m->l;"),
    ("case n of Z->Z;S m->l;", ("l", Var "$0"), "case n of Z->Z;S m->$0;"),
    ("case n of Z->Z;S m->m;", ("m", Var "$0"), "case n of Z->Z;S m->m;")
  ] ++
  map (\(a, e) -> "freeVars" ~: a ~: (freeVars . parseExpr) a ~?= e)
  [
    ("x", ["x"]),
    ("f x", ["f", "x"]),
    ("f (g x) (h x y)", ["f", "g", "x", "h", "y"]),
    ("{n->Succ n}", []),
    ("Succ n", ["n"]),
    ("Branch t t", ["t"]),
    ("let inc={n->S n} in inc m", ["m"]),
    ("let inc={n->S n} in inc n", ["n"]),
    ("let inc={n->S m} in inc n", ["n", "m"]),
    ("let inc={n->(let s=Succ in s n)} in inc m", ["m"]),
    ("let inc={n->(let s=Succ in s n)} in inc n", ["n"]),
    ("let inc={n->s (let s=A in s m)} in inc m", ["m", "s"])
  ]
