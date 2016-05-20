
module Main (main) where

import Test.HUnit (Counts, runTestTT, test, (~:), (~?=))

import Expr (Expr(..), Pat(Pat), con, app, zero, suc, cons, nil)
import Parser (parseExpr)

main :: IO Counts
main = runTestTT $ test $
  map (\(a, e) -> "parseExpr" ~: a ~: parseExpr a ~?= e)
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
    ("let cp={a->case a of Zero->0; Succ aa->Succ (cp aa);} in cp",
      Let "cp" (Lam "a" (Case (Var "a") [
        (Pat "Zero" [],
          Con "Zero" []),
        (Pat "Succ" ["aa"],
          App (Con "Succ" []) (App (Var "cp") (Var "aa")))
      ])) (Var "cp")),
    ("case var of True -> False;",
      Case (Var "var") [
        (Pat "True" [], con "False")
      ]),
    ("case var of True -> False; False -> True;",
      Case (Var "var") [
        (Pat "True" [], con "False"),
        (Pat "False" [], con "True")
      ]),
    ("case var of True -> False; False -> True; Just n -> m;",
      Case (Var "var") [
        (Pat "True" [], con "False"),
        (Pat "False" [], con "True"),
        (Pat "Just" ["n"], Var "m")
      ]),
    ("[]", nil),
    ("  [  ]  ", nil),
    ("[True]", app cons [con "True", nil]),
    ("[A, B]", app cons [con "A", app cons [con "B", nil]]),
    ("[False, True, False, True]",
      App (App cons (con "False")) (
        App (App cons (con "True")) (
          App (App cons (con "False")) (
            App (App cons (con "True")) nil))) ),
    ("[x]", App (App cons (Var "x")) nil),
    ("[x,y]",
      App (App cons (Var "x")) (
        App (App cons (Var "y")) nil)),
    ("[x,One,y,Two]",
      App (App cons (Var "x")) (
        App (App cons (con "One")) (
          App (App cons (Var "y")) (
            App (App cons (con "Two")) nil))) )
  ]
