
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(Var, Lam, Let, App, Case), Pat(Pat),
  con, app, appVars, let1, true, false, zero, suc, cons, nil, bool, nat, list)
import qualified Parser (parseExpr)
import qualified FastParser (parseExpr)

trunc :: Int -> String -> String
trunc n s = if length s <= n then s else take n s ++ "[...]"

testParser :: String -> (String -> Expr) -> TestTree
testParser msg parseExpr' = testGroup (msg ++ ".parseExpr str ~> expr") [
    go "x" $ Var "x",
    go "$x" $ Var "$x",
    go "$x__" $ Var "$x__",
    go "$x__0" $ Var "$x__0",
    go "$x1" $ Var "$x1",
    go "var" $ Var "var",
    go "xs'" $ Var "xs'",
    go "xs'123'ab" $ Var "xs'123'ab",
    go "$xs'_" $ Var "$xs'_",
    go "var234" $ Var "var234",
    go "$var" $ Var "$var",
    go "veryverylonglongvar" $ Var "veryverylonglongvar",
    go "0" zero,
    go "Zero" zero,
    go "1" $ nat 1,
    go "3" $ nat 3,
    go "5" $ nat 5,
    go "Nil" nil,
    go "Cons" cons,
    go "Succ Zero" $ nat 1,
    go "(Succ Zero)" $ nat 1,
    go "((Succ) (Zero))" $ nat 1,
    go "Succ (Succ Zero)" $ nat 2,
    go "Cons Zero Nil" $ list nat [0],
    go "Cons 2 (Cons 0 Nil)" $ list nat [2, 0],
    go "f x" $ App (Var "f") (Var "x"),
    go "f x y" $ appVars (Var "f") ["x", "y"],
    go "f var y" $ appVars (Var "f") ["var", "y"],
    go "Succ n" $ App suc (Var "n"),
    go "let x=Nil in x" $ let1 "x" nil (Var "x"),
    go "{x->x}" $ Lam "x" (Var "x"),
    go "{$x->$x}" $ Lam "$x" (Var "$x"),
    go "{$x0->$x0}" $ Lam "$x0" (Var "$x0"),
    go "{var->var}" $ Lam "var" (Var "var"),
    go "{x->True}" $ Lam "x" true,
    go "{x->2}" $ Lam "x" (nat 2),
    go "{f->{x->f x}}" $ Lam "f" (Lam "x" (App (Var "f") (Var "x"))),
    go "let x={y->y} in x" $ let1 "x" (Lam "y" (Var "y")) (Var "x"),
    go "let $x={y->y} in $x" $ let1 "$x" (Lam "y" (Var "y")) (Var "$x"),
    go "let id={y->y} in id" $ let1 "id" (Lam "y" (Var "y")) (Var "id"),
    go "let x=0 in Succ x" $ let1 "x" zero (App suc (Var "x")),
    go "let $v_0=A in $v_0" $ let1 "$v_0" (con "A") (Var "$v_0"),
    go "let $v_0 = A in $v_0" $ let1 "$v_0" (con "A") (Var "$v_0"),
    go "let x=A;y=B;z=C in x y z" $
      Let [("x", con "A"), ("y", con "B"), ("z", con "C")]
        (appVars (Var "x") ["y", "z"]),
    go "[]" nil,
    go "  [  ]  " nil,
    go "[True]" $ list bool [True],
    go "[A, B]" $ list id [con "A", con "B"],
    go "[A,B,C]" $ list id [con "A", con "B", con "C"],
    go "[x]" $ list id [Var "x"],
    go "[x,y]" $ list id [Var "x", Var "y"],
    go "[x,A,y]" $ list id [Var "x", con "A", Var "y"],
    go "x:xs" $ appVars cons ["x", "xs"],
    go "(x:xs)" $ appVars cons ["x", "xs"],
    go "A:Nil" $ list id [con "A"],
    go "A:(B:Nil)" $ list id [con "A", con "B"],
    go "A:(B:(C:Nil))" $ list id [con "A", con "B", con "C"],
    go "A:B:Nil" $ list id [con "A", con "B"],
    go "A:B:C:Nil" $ list id [con "A", con "B", con "C"],
    go "A:B:C:D:Nil" $ list id [con "A", con "B", con "C", con "D"],
    go "A:B:xs" $ app cons [con "A", app cons [con "B", Var "xs"]],
    go "case var of True -> False;" $
      Case (Var "var") [
        (Pat "True" [], false)
      ],
    go "case var of Cons x_ xs_ -> xs_;" $
      Case (Var "var") [
        (Pat "Cons" ["x_", "xs_"], Var "xs_")
      ],
    go "case var of Cons x' xs' -> xs';" $
      Case (Var "var") [
        (Pat "Cons" ["x'", "xs'"], Var "xs'")
      ],
    go "case var of True -> False; False -> True;" $
      Case (Var "var") [
        (Pat "True" [], false),
        (Pat "False" [], true)
      ],
    go "case xs of True -> False; False -> True; Just n -> m;" $
      Case (Var "xs") [
        (Pat "True" [], false),
        (Pat "False" [], true),
        (Pat "Just" ["n"], Var "m")
      ],
    go "let c=case n of Z->A;S m->B;;d=case m of B l r->C;T->D; in c d" $
      Let [("c", Case (Var "n") [
        (Pat "Z" [], con "A"),
        (Pat "S" ["m"], con "B")
      ]), ("d", Case (Var "m") [
        (Pat "B" ["l", "r"], con "C"),
        (Pat "T" [], con "D")
      ])] (App (Var "c") (Var "d")),
    go "let c=case n of Z->A;S m->B; in c" $
      Let [("c", Case (Var "n") [
        (Pat "Z" [], con "A"),
        (Pat "S" ["m"], con "B")
      ])] (Var "c")
  ]
  where go a e = testCase (trunc 70 (a ++ " ~> " ++ show e)) $ parseExpr' a @?= e

main :: IO ()
main = defaultMain $ testGroup "Parser" [
    testParser "Parser" Parser.parseExpr,
    testParser "FastParser" FastParser.parseExpr
  ]
