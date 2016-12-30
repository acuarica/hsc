
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(Var, Lam, Let, App, Case), Pat(Pat),
  con, app, appVars, let1,
  true, false, zero, suc, cons, nil, bool, nat, list)
import qualified Parser (parseExpr)
import qualified FastParser (parseExpr)

testParser :: String -> (String -> Expr) -> TestTree
testParser msg parseExpr' = testGroup (msg ++ ".parseExpr str ~> expr") $
  map (\(a, e) -> testCase (a ++ " ~> " ++ show e) $ parseExpr' a @?= e)
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
    ("Cons Zero Nil", list nat [0]),
    ("Cons 2 (Cons 0 Nil)", list nat [2, 0]),
    ("f x", App (Var "f") (Var "x")),
    ("f x y", appVars (Var "f") ["x", "y"]),
    ("f var y", appVars (Var "f") ["var", "y"]),
    ("Succ n", App suc (Var "n")),
    ("let x=Nil in x", let1 "x" nil (Var "x")),
    ("{x->x}", Lam "x" (Var "x")),
    ("{$x->$x}", Lam "$x" (Var "$x")),
    ("{$x0->$x0}", Lam "$x0" (Var "$x0")),
    ("{var->var}", Lam "var" (Var "var")),
    ("{x->True}", Lam "x" true),
    ("{x->2}", Lam "x" (nat 2)),
    ("{f->{x->f x}}", Lam "f" (Lam "x" (App (Var "f") (Var "x")))),
    ("let x={y->y} in x", let1 "x" (Lam "y" (Var "y")) (Var "x")),
    ("let $x={y->y} in $x", let1 "$x" (Lam "y" (Var "y")) (Var "$x")),
    ("let id={y->y} in id", let1 "id" (Lam "y" (Var "y")) (Var "id")),
    ("let x=0 in Succ x", let1 "x" zero (App suc (Var "x"))),
    ("let $v_0=A in $v_0", let1 "$v_0" (con "A") (Var "$v_0")),
    ("let $v_0 = A in $v_0", let1 "$v_0" (con "A") (Var "$v_0")),
    ("let x=A;y=B;z=C in x y z",
      Let [("x", con "A"), ("y", con "B"), ("z", con "C")]
        (appVars (Var "x") ["y", "z"])),
    ("[]", nil),
    ("  [  ]  ", nil),
    ("[True]", list bool [True]),
    ("[A, B]", list id [con "A", con "B"]),
    ("[A,B,C]", list id [con "A", con "B", con "C"]),
    ("[x]", list id [Var "x"]),
    ("[x,y]", list id [Var "x", Var "y"]),
    ("[x,A,y]", list id [Var "x", con "A", Var "y"]),
    ("x:xs", appVars cons ["x", "xs"]),
    ("(x:xs)", appVars cons ["x", "xs"]),
    ("A:Nil", list id [con "A"]),
    ("A:(B:Nil)", list id [con "A", con "B"]),
    ("A:(B:(C:Nil))", list id [con "A", con "B", con "C"]),
    ("A:B:Nil", list id [con "A", con "B"]),
    ("A:B:C:Nil", list id [con "A", con "B", con "C"]),
    ("A:B:C:D:Nil", list id [con "A", con "B", con "C", con "D"]),
    ("A:B:xs", app cons [con "A", app cons [con "B", Var "xs"]]),
    ("let c=case n of Z->A;S m->B;;d=case m of B l r->C;T->D; in c d",
      Let [("c", Case (Var "n") [
        (Pat "Z" [], con "A"),
        (Pat "S" ["m"], con "B")
      ]), ("d", Case (Var "m") [
        (Pat "B" ["l", "r"], con "C"),
        (Pat "T" [], con "D")
      ])] (App (Var "c") (Var "d"))),
    ("let c=case n of Z->A;S m->B; in c",
      Let [("c", Case (Var "n") [
        (Pat "Z" [], con "A"),
        (Pat "S" ["m"], con "B")
      ])] (Var "c")),
    ("case var of True -> False;",
      Case (Var "var") [
        (Pat "True" [], false)
      ]),
    ("case var of Cons x_ xs_ -> xs_;",
      Case (Var "var") [
        (Pat "Cons" ["x_", "xs_"], Var "xs_")
      ]),
    ("case var of Cons x' xs' -> xs';",
      Case (Var "var") [
        (Pat "Cons" ["x'", "xs'"], Var "xs'")
      ]),
    ("case var of True -> False; False -> True;",
      Case (Var "var") [
        (Pat "True" [], false),
        (Pat "False" [], true)
      ]),
    ("case xs of True -> False; False -> True; Just n -> m;",
      Case (Var "xs") [
        (Pat "True" [], false),
        (Pat "False" [], true),
        (Pat "Just" ["n"], Var "m")
      ])
  ]

main :: IO ()
main = defaultMain $ testGroup "Parser/FastParser testing" [
    testParser "Parser" Parser.parseExpr,
    testParser "FastParser" FastParser.parseExpr
  ]
