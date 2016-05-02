
module Main where

import System.Exit
import Test.HUnit

import Expr
import Parser
import Pretty
import Util

doPrint :: (Expr, Expr, Expr) -> String
doPrint (expr, expexpr, actexpr) =
  pprint expr ++ " ~~> " ++ pprint actexpr

doEval :: (Expr, Expr) -> (Expr, Expr, Expr)
doEval (expr, expexpr) = (expr, expexpr, eval expr)

main :: IO ()
main = ((\ts -> doPrints doPrint ts >> doTests doTest ts) . map doEval) [
    (x, Var "x" True),
    (var, Var "var" True),
    (true, true),
    (false, false),
    (zero, zero),
    (suc, suc),
    (nil, nil),
    (cons, cons),
    (App suc zero, one),
    (App suc n, Con "Succ" [Var "n" True]),
    (App suc (App suc zero), two),
    (App cons zero, Con "Cons" [zero]),
    (App (Con "Cons" [zero]) nil, Con "Cons" [zero, nil]),
    (App (App cons zero) nil, Con "Cons" [zero, nil]),
    (App (App cons false) (App (App cons true) nil),
      Con "Cons" [false, Con "Cons" [true, nil]]),
    (App (App cons two) (App (App cons one) (App (App cons zero) nil)),
      Con "Cons" [two, Con "Cons" [one, Con "Cons" [zero, nil]]]),
    (App (Lam "x" x) zero, zero),
    (Let "x" zero x, zero),
    (Let "x" (Lam "y" y) (Let "z" nil (App x z)), nil),
    (Let "x" true (Case x [(false, true), (true, false)]), false),
    (Let "x" (Lam "y" (Case y [(false, true), (true, false)]))
        (Let "z" true (App x z)),
      false),
    (Let "iszero" (Lam "n" (Case n [(zero, true), (App suc m, false)]))
        (Let "x" two (App iszero x)),
      false),
    (Let "iszero" (Lam "n" (Case n [(zero, true), (App suc m, false)]))
        (Let "x" zero (App iszero x)),
      true),
    (Let "plus1" (Lam "n" (App suc n)) (Let "x" one (App plus1 x)), two),
    (Let "and" (Lam "n" (Lam "m" (Case n [
        (false, false),
        (true, m)
      ]))) (App (App (newvar "and") true) true),
      true),
    (Let "pred" (Lam "n" (Case n [
        (zero, zero),
        (App suc n', n')
      ])) (App (newvar "pred") zero),
      zero),
    (Let "pred" (Lam "n" (Case n [
        (zero, zero),
        (App suc n', n')
      ])) (App (newvar "pred") two),
      one),
    (Let "plus" (Lam "n" (Lam "m" (Case n [
        (zero, m),
        (App suc n', App (App plus n') (App suc m))
      ]))) (App (App plus three) two),
      five),
    (Let "x" (Con "Zero" []) (App (Con "Succ" []) (newvar "x")),
      Con "Succ" [Con "Zero" []])
  ]
  where
    x = newvar "x"
    y = newvar "y"
    z = newvar "z"
    var = newvar "var"
    n = newvar "n"
    n' = newvar "n'"
    m = newvar "m"
    m' = newvar "m'"
    iszero = newvar "iszero"
    plus1 = newvar "plus1"
    plus = newvar "plus"
    one = Con "Succ" [zero]
    two = Con "Succ" [one]
    three = Con "Succ" [two]
    four = Con "Succ" [three]
    five = Con "Succ" [four]
