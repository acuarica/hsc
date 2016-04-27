
module Main where

import Test.HUnit

import Expr
import Parser

testCase :: (String, Expr) -> Test
testCase (code, expected) = TestCase (
    assertEqual ("For " ++ code) expected (parseWith exprp code)
  )

main :: IO Counts
main = runTestTT (TestList (map testCase [
    ("$x", Var "$x"),
    ("$var", Var "$var"),
    ("0", Con "Zero" []),
    ("1", Con "Succ" [Con "Zero" []]),
    ("3", Con "Succ" [Con "Succ" [Con "Succ" [Con "Zero" []]]]),
    ("T", Con "T" []),
    ("True", Con "True" []),
    ("False", Con "False" []),
    ("Zero", Con "Zero" []),
    ("Succ Zero", App (Con "Succ" []) (Con "Zero" [])),
    ("(Succ Zero)", App (Con "Succ" []) (Con "Zero" [])),
    ("((Succ) (Zero))", App (Con "Succ" []) (Con "Zero" [])),
    ("Succ Succ Zero",
      App (App (Con "Succ" []) (Con "Succ" [])) (Con "Zero" [])),
    ("Nil", Con "Nil" []),
    ("Cons True Nil",
      App (App (Con "Cons" []) (Con "True" [])) (Con "Nil" [])),
    ("Cons False (Cons True Nil)",
      App (App (Con "Cons" []) (Con "False" []))
          (App (App (Con "Cons" []) (Con "True" [])) (Con "Nil" []))),
    ("Succ $n", App (Con "Succ" []) (Var "$n")),
    ("{\\$x->$x}", Lam "$x" (Var "$x")),
    ("{\\$var->$var}", Lam "$var" (Var "$var")),
    ("{\\$x->True}", Lam "$x" (Con "True" [])),
    ("{\\$x->2}", Lam "$x" (Con "Succ" [Con "Succ" [Con "Zero" []]])),
    ("let $x={\\$y->$y} in $x", Let "$x" (Lam "$y" (Var "$y")) (Var "$x")),
    ("let $var={\\$y->$y} in $var",
      Let "$var" (Lam "$y" (Var "$y")) (Var "$var")),
    ("case $x of { True -> False; }",
      Case (Var "$x") [
        (Con "True" [], Con "False" [])
      ]),
    ("case $var of { True -> False; }",
      Case (Var "$var") [
        (Con "True" [], Con "False" [])
      ]),
    ("case $var of { True -> False; False -> True; }",
      Case (Var "$var") [
        (Con "True" [], Con "False" []),
        (Con "False" [], Con "True" [])
      ]),
    ("case $var of { True -> False; False -> True; $n -> $m; }",
      Case (Var "$var") [
        (Con "True" [], Con "False" []),
        (Con "False" [], Con "True" []),
        (Var "$n", Var "$m")
      ])
  ]))
