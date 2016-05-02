
module Main where

import Test.HUnit

import Expr
import Parser
import Util

doParse :: (String, Expr) -> (String, Expr, Expr)
doParse (code, expected) = (code, expected, parseExpr code)

main :: IO ()
main = doTests (doTest . doParse)  [
    ("$x", newvar "$x"),
    ("$var", newvar "$var"),
    ("0", zero),
    ("Zero", zero),
    ("1", App suc zero),
    ("3", App suc (App suc (App suc zero))),
    ("True", true),
    ("False", false),
    ("Succ Zero", App suc zero),
    ("(Succ Zero)", App suc zero),
    ("((Succ) (Zero))", App suc zero),
    ("Succ (Succ Zero)", App suc (App suc zero)),
    ("Nil", nil),
    ("Cons True Nil", App (App cons true) nil),
    ("Cons False (Cons True Nil)",
      App (App (Con "Cons" []) (Con "False" []))
          (App (App (Con "Cons" []) (Con "True" [])) (Con "Nil" []))),
    ("Succ $n", App (Con "Succ" []) (newvar "$n")),
    ("{\\$x->$x}", Lam "$x" (newvar "$x")),
    ("{\\$var->$var}", Lam "$var" (newvar "$var")),
    ("{\\$x->True}", Lam "$x" (Con "True" [])),
    ("{\\$x->2}", Lam "$x" (App suc (App suc zero))),
    ("let $x={\\$y->$y} in $x",
      Let "$x" (Lam "$y" (newvar "$y")) (newvar "$x")),
    ("let $var={\\$y->$y} in $var",
      Let "$var" (Lam "$y" (newvar "$y")) (newvar "$var")),
    ("case $x of { True -> False; }",
      Case (newvar "$x") [
        (Con "True" [], Con "False" [])
      ]),
    ("case $var of { True -> False; }",
      Case (newvar "$var") [
        (Con "True" [], Con "False" [])
      ]),
    ("case $var of { True -> False; False -> True; }",
      Case (newvar "$var") [
        (Con "True" [], Con "False" []),
        (Con "False" [], Con "True" [])
      ]),
    ("case $var of { True -> False; False -> True; $n -> $m; }",
      Case (newvar "$var") [
        (Con "True" [], Con "False" []),
        (Con "False" [], Con "True" []),
        (newvar "$n", newvar "$m")
      ]),
    ("[]", nil),
    ("  [  ]  ", nil),
    ("[True]", App (App cons true) nil),
    ("[False, True]", App (App cons false) (App (App cons true) nil)),
    ("[False, True, False]",
      App (App cons false) (
        App (App cons true) (
          App (App cons false) nil))  ),
    ("[False, True, False, True]",
      App (App cons false) (
        App (App cons true) (
          App (App cons false) (
            App (App cons true) nil))) ),
    ("[$x]", App (App cons (newvar "$x")) nil),
    ("[$x,$y]",
      App (App cons (newvar "$x")) (App (App cons (newvar "$y")) nil)),
    ("[$x,One,$y,Two]",
      App (App cons (newvar "$x")) (
        App (App cons (Con "One" [])) (
          App (App cons (newvar "$y")) (
            App (App cons (Con "Two" [])) nil))) ),
    ("let $x=0 in Succ $x", Let "$x" zero (App suc (newvar "$x")))
  ]
