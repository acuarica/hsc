
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(..), Pat(Pat), con, app, zero, suc, cons, nil)
import Parser (parseExpr)

testParser :: TestTree
testParser = testGroup "Parser.parseExpr str ~~> expr" $
  map (\(a, e) -> 
    testCase (a ++ " ~~> " ++ show e) $
      parseExpr a @?= e)
  [
    ("x", Var "x"),
    ("$x", Var "$x"),
    ("$x__", Var "$x__"),
    ("$x__0", Var "$x__0"),
    ("$x1", Var "$x1"),
    ("var", Var "var"),
    ("var234", Var "var234"),
    ("$var", Var "$var"),
    ("veryverylonglongvar", Var "veryverylonglongvar"),
    ("0", zero),
    ("Zero", zero),
    ("1", App suc zero),
    ("3", App suc (App suc (App suc zero))),
    ("Nil", nil),
    ("Cons", cons),
    ("Succ Zero", App suc zero),
    ("(Succ Zero)", App suc zero),
    ("((Succ) (Zero))", App suc zero),
    ("Succ (Succ Zero)", App suc (App suc zero)),
    ("Nil", nil),
    ("Cons Zero Nil", App (App cons zero) nil),
    ("Cons 0 (Cons 0 Nil)", app cons [zero, app cons [zero, nil]]),
    ("f x", App (Var "f") (Var "x")),
    ("f x y", App (App (Var "f") (Var "x")) (Var "y")),
    ("f var y", App (App (Var "f") (Var "var")) (Var "y")),
    ("Succ n", App suc (Var "n")),
    ("let x=Nil in x", Let "x" nil (Var "x")),
    ("{x->x}", Lam "x" (Var "x")),
    ("{$x->$x}", Lam "$x" (Var "$x")),
    ("{$x0->$x0}", Lam "$x0" (Var "$x0")),
    ("{var->var}", Lam "var" (Var "var")),
    ("{x->True}", Lam "x" (Con "True" [])),
    ("{x->2}", Lam "x" (App suc (App suc zero))),
    ("{f->{x->f x}}", Lam "f" (Lam "x" (App (Var "f") (Var "x")))),
    ("let x={y->y} in x", Let "x" (Lam "y" (Var "y")) (Var "x")),
    ("let $x={y->y} in $x", Let "$x" (Lam "y" (Var "y")) (Var "$x")),
    ("let $x0={y->y} in $x0", Let "$x0" (Lam "y" (Var "y")) (Var "$x0")),
    ("let id={y->y} in id", Let "id" (Lam "y" (Var "y")) (Var "id")),
    ("let x=0 in Succ x", Let "x" zero (App suc (Var "x"))),
    ("[]", nil),
    ("  [  ]  ", nil),
    ("[True]", app cons [con "True", nil]),
    ("[A, B]", app cons [con "A", app cons [con "B", nil]]),
    ("[A,B,C]", app cons [con "A", app cons [con "B", app cons [con "C", nil]]]),
    ("[x]", app cons [Var "x", nil]),
    ("[x,y]", app cons [Var "x", app cons [Var "y", nil]]),
    ("[x,A,y]", app cons [Var "x", app cons [con "A", app cons [Var "y", nil]]]),
    ("let f={a->case a of Z->A;S b->B;} in f",
      Let "f" (Lam "a" (Case (Var "a") [
        (Pat "Z" [], Con "A" []),
        (Pat "S" ["b"], Con "B" [])
      ])) (Var "f")),
    ("case var of True -> False;",
      Case (Var "var") [
        (Pat "True" [], con "False")
      ]),
    ("case var of T -> F; F -> T;",
      Case (Var "var") [
        (Pat "T" [], con "F"),
        (Pat "F" [], con "T")
      ]),
    ("case var of T -> F; F -> T; Just n -> m;",
      Case (Var "var") [
        (Pat "T" [], con "F"),
        (Pat "F" [], con "T"),
        (Pat "Just" ["n"], Var "m")
      ])
  ]

main :: IO ()
main = defaultMain testParser 
