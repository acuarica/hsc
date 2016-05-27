
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(Var), app, appVars, subst, freeVars)
import Parser (parseExpr)

main :: IO ()
main = defaultMain $ testGroup "Expr: app/appVars/subst/freeVars"
  [testGroup "app expr [expr] ~~> expr" $
  map (\(f, args, e) ->
    testCase (f ++ " [" ++ unwords args ++ "] ~~> " ++ e) $
      app (parseExpr f) (map parseExpr args) @?= parseExpr e)
  [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y")
  ], testGroup "appVars expr [var] ~~> expr" $
  map (\(f, args, e) ->
    testCase (f ++ " [" ++ unwords args ++ "] ~~> " ++ e) $
      appVars (parseExpr f) args @?= parseExpr e) [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y")
  ], testGroup "subst (var, expr) expr ~~> expr" $
  map (\(a,s,e)-> testCase (a ++ show s ++ " ~~> " ++ e) $
    subst s (parseExpr a) @?= parseExpr e)
  [
    ("{n->m}", ("m", Var "$0"), "{n->$0}"),
    ("{n->n}", ("n", Var "$0"), "{n->n}"),
    ("let n=A in m", ("m", Var "$0"), "let n=A in $0"),
    ("let n=A in n", ("n", Var "$0"), "let n=A in n"),
    ("case n of Z->Z;S m->l;", ("n", Var "$0"), "case $0 of Z->Z;S m->l;"),
    ("case n of Z->Z;S m->l;", ("l", Var "$0"), "case n of Z->Z;S m->$0;"),
    ("case n of Z->Z;S m->m;", ("m", Var "$0"), "case n of Z->Z;S m->m;")
  ], testGroup "freeVars expr ~~> [Var]" $
  map (\(a, e) -> testCase (a ++ " ~~> " ++ unwords e) $
    (freeVars . parseExpr) a @?= e)
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
  ]
