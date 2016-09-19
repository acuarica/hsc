
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(Var), app, appVars, subst, freeVars, alpha)
import Parser (parseExpr)

appTest :: TestTree
appTest = testGroup "app expr [expr] ~~> expr" $
  map (\(f, args, e) ->
    testCase (f ++ " " ++ show (map parseExpr args) ++ " ~~> " ++ e) $
      app (parseExpr f) (map parseExpr args) @?= parseExpr e)
  [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y"),
    ("Cons", ["x", "xs"], "Cons x xs"),
    ("Cons", ["x", "Cons y []"], "Cons x (Cons y [])")
  ]

appVarsTest :: TestTree
appVarsTest = testGroup "appVars expr [var] ~~> expr" $
  map (\(f, args, e) ->
    testCase (f ++ " " ++ show (map parseExpr args) ++ " ~~> " ++ e) $
      appVars (parseExpr f) args @?= parseExpr e) [
    ("{f->{x->f x}}", ["g", "y"], "{f->{x->f x}} g y"),
    ("Cons", ["x", "xs"], "Cons x xs"),
    ("{t->Tree}", ["l", "x", "r"], "{t->Tree} l x r")
  ]

substTest :: TestTree
substTest = testGroup "subst (var, expr) expr ~~> expr" $
  map (\(a,s,e)->
    testCase (a ++ show s ++ " ~~> " ++ e) $
      subst s (parseExpr a) @?= parseExpr e)
  [
    ("{n->m}", ("m", Var "$0"), "{n->$0}"),
    ("{n->n}", ("n", Var "$0"), "{n->n}"),
    ("let n=A in m", ("m", Var "$0"), "let n=A in $0"),
    ("let n=A in n", ("n", Var "$0"), "let n=A in n"),
    ("case n of Z->Z;S m->l;", ("n", Var "$0"), "case $0 of Z->Z;S m->l;"),
    ("case n of Z->Z;S m->l;", ("l", Var "$0"), "case n of Z->Z;S m->$0;"),
    ("case n of Z->Z;S m->m;", ("m", Var "$0"), "case n of Z->Z;S m->m;")
  ]

freeVarsTest :: TestTree
freeVarsTest = testGroup "freeVars expr ~~> [Var]" $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ unwords e) $
      (freeVars . parseExpr) a @?= e)
  [
    ("x", ["x"]),
    ("f x", ["f", "x"]),
    ("f (g x) (h x y)", ["f", "g", "x", "h", "y"]),
    ("{n->Succ n}", []),
    ("{x->Cons x xs}", ["xs"]),
    ("{x->Cons x xs} x", ["xs", "x"]),
    ("Succ n", ["n"]),
    ("Cons x xs", ["x", "xs"]),
    ("Branch t t", ["t"]),
    ("let inc={n->S n} in inc m", ["m"]),
    ("let inc={n->S n} in inc n", ["n"]),
    ("let inc={n->S m} in inc n", ["n", "m"]),
    ("let inc={n->(let s=Succ in s n)} in inc m", ["m"]),
    ("let inc={n->(let s=Succ in s n)} in inc n", ["n"]),
    ("let inc={n->s (let s=A in s m)} in inc m", ["m", "s"])
  ]

alphaTest :: TestTree
alphaTest = testGroup "alpha expr ~~> expr" $
  map (\(a, e) ->
    testCase (a ++ " <~~> " ++ e) $
      (alpha . parseExpr) a @?= (alpha . parseExpr) e)
  [
    ("f x", "f x"),
    ("let x=A in x", "let y=A in y"),
    ("let x=A in let y=B in x y", "let u=A in let v=B in u v"),
    ("(let x=F in x) (let x=X in x)", "(let y=F in y) (let z=X in z)"),
    ("F (let x=A in x)", "F (let y=A in y)"),
    ("case x of X -> (let y=A in y);", "case x of X -> (let z=A in z);"),
    ("case (let x=a in x) of X->A;", "case (let y=a in y) of X->A;")
  ]

main :: IO ()
main = defaultMain $ testGroup "Expr"
  [appTest, appVarsTest, substTest, freeVarsTest, alphaTest]
