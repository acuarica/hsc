
module Main where

import Test.HUnit

import Expr
import Parser
import Util

doParse :: (String, Expr) -> (String, Expr, Expr)
doParse (code, expected) = (code, expected, parseExpr code)

main :: IO ()
main = doTests doParse  [
    ("x", usevar "x"),
    ("var", usevar "var"),
    ("veryverylonglongvar", usevar "veryverylonglongvar"),
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
    ("Cons 0 (Cons 0 Nil)", App (App cons zero) (App (App cons zero) nil)),
    ("f x", App (usevar "f") (usevar "x")),
    ("f x y", App (App (usevar "f") (usevar "x")) (usevar "y")),
    ("f var y", App (App (usevar "f") (usevar "var")) (usevar "y")),
    ("Succ n", App suc (usevar "n")),
    ("let x=True in x", Let "x" true (usevar "x")),
    ("{x->x}", Lam "x" (usevar "x")),
    ("{var->var}", Lam "var" (usevar "var")),
    ("{x->True}", Lam "x" true),
    ("{x->2}", Lam "x" (App suc (App suc zero))),
    ("{f->{x->f x}}", Lam "f" (Lam "x" (App (usevar "f") (usevar "x")))),
    ("let x={y->y} in x", Let "x" (Lam "y" (usevar "y")) (usevar "x")),
    ("let id={y->y} in id", Let "id" (Lam "y" (usevar "y")) (usevar "id")),
    ("let x=0 in Succ x", Let "x" zero (App suc (usevar "x"))),
    ("case x of True -> False;",
      Case (usevar "x") [
        (true, false)
      ] False),
    ("case var of True -> False;",
      Case (usevar "var") [
        (true, false)
      ] False),
    ("case var of True -> False; False -> True;",
      Case (usevar "var") [
        (true, false),
        (false, true)
      ] False),
    ("case var of True -> False; False -> True; n -> m;",
      Case (usevar "var") [
        (true, false),
        (false, true),
        (usevar "n", usevar "m")
      ] False),
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
    ("[x]", App (App cons (usevar "x")) nil),
    ("[x,y]",
      App (App cons (usevar "x")) (
        App (App cons (usevar "y")) nil)),
    ("[x,One,y,Two]",
      App (App cons (usevar "x")) (
        App (App cons (Con "One" [])) (
          App (App cons (usevar "y")) (
            App (App cons (Con "Two" [])) nil))) )
  ]
