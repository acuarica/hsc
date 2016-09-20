
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(..), Pat(Pat),
  con, app, appVars, zero, suc, cons, nil, nat, list)
import Parser (parseExpr)

testParser :: TestTree
testParser = testGroup "Parser.parseExpr str ~> expr" $
  map (\(a, e) ->
    testCase (a ++ " ~> " ++ show e) $
      parseExpr a @?= e)
  [
    ("x", Var "x"),
    ("$x", Var "$x"),
    ("$x__", Var "$x__"),
    ("$x__0", Var "$x__0"),
    ("$x1", Var "$x1"),
    ("var", Var "var"),
    ("xs'", Var "xs'"),
    ("xs'123'ab", Var "xs'123'ab"),
    ("$xs'_", Var "$xs'_"),
    ("var234", Var "var234"),
    ("$var", Var "$var"),
    ("veryverylonglongvar", Var "veryverylonglongvar"),
    ("0", zero),
    ("Zero", zero),
    ("1", nat 1),
    ("3", nat 3),
    ("5", nat 5),
    ("Nil", nil),
    ("Cons", cons),
    ("Succ Zero", nat 1),
    ("(Succ Zero)", nat 1),
    ("((Succ) (Zero))", nat 1),
    ("Succ (Succ Zero)", nat 2),
    ("Cons Zero Nil", list [zero]),
    ("Cons 2 (Cons 0 Nil)", list [nat 2, zero]),
    ("f x", App (Var "f") (Var "x")),
    ("f x y", appVars (Var "f") ["x", "y"]),
    ("f var y", appVars (Var "f") ["var", "y"]),
    ("Succ n", App suc (Var "n")),
    ("let x=Nil in x", Let "x" nil (Var "x")),
    ("{x->x}", Lam "x" (Var "x")),
    ("{$x->$x}", Lam "$x" (Var "$x")),
    ("{$x0->$x0}", Lam "$x0" (Var "$x0")),
    ("{var->var}", Lam "var" (Var "var")),
    ("{x->True}", Lam "x" (Con "True" [])),
    ("{x->2}", Lam "x" (nat 2)),
    ("{f->{x->f x}}", Lam "f" (Lam "x" (App (Var "f") (Var "x")))),
    ("let x={y->y} in x", Let "x" (Lam "y" (Var "y")) (Var "x")),
    ("let $x={y->y} in $x", Let "$x" (Lam "y" (Var "y")) (Var "$x")),
    ("let $x0={y->y} in $x0", Let "$x0" (Lam "y" (Var "y")) (Var "$x0")),
    ("let id={y->y} in id", Let "id" (Lam "y" (Var "y")) (Var "id")),
    ("let x=0 in Succ x", Let "x" zero (App suc (Var "x"))),
    ("let $v_0=A in $v_0", Let "$v_0" (con "A") (Var "$v_0")),
    ("let $v_0 = A in $v_0", Let "$v_0" (con "A") (Var "$v_0")),
    ("[]", nil),
    ("  [  ]  ", nil),
    ("[True]", list [con "True"]),
    ("[A, B]", list [con "A", con "B"]),
    ("[A,B,C]", list [con "A", con "B", con "C"]),
    ("[x]", list [Var "x"]),
    ("[x,y]", list [Var "x", Var "y"]),
    ("[x,A,y]", list [Var "x", con "A", Var "y"]),
    ("x:xs", app cons [Var "x", Var "xs"]),
    ("(x:xs)", app cons [Var "x", Var "xs"]),
    ("A:Nil", app cons [con "A", nil]),
    --("A:B:C:Nil", app cons [con "A",
    --app cons [con "B", app cons [con "C", nil]]]),
    ("let c=case n of Z->A;S m->B; in c",
      Let "c" (Case (Var "n") [
        (Pat "Z" [], Con "A" []),
        (Pat "S" ["m"], Con "B" [])
      ]) (Var "c")),
    ("case var of True -> False;",
      Case (Var "var") [
        (Pat "True" [], con "False")
      ]),
    ("case var of Cons x_ xs_ -> xs_;",
      Case (Var "var") [
        (Pat "Cons" ["x_", "xs_"], Var "xs_")
      ]),
    ("case var of Cons x' xs' -> xs';",
      Case (Var "var") [
        (Pat "Cons" ["x'", "xs'"], Var "xs'")
      ]),
    ("case var of T -> F; F -> T;",
      Case (Var "var") [
        (Pat "T" [], con "F"),
        (Pat "F" [], con "T")
      ]),
    ("case xs of T -> F; F -> T; Just n -> m;",
      Case (Var "xs") [
        (Pat "T" [], con "F"),
        (Pat "F" [], con "T"),
        (Pat "Just" ["n"], Var "m")
      ])
  ]

main :: IO ()
main = defaultMain testParser
