
module Main where

import Expr
import Pretty
import Eval
import Util

doEval :: (Expr, Expr) -> (String, Expr, Expr)
doEval (expr, expexpr) = (show expr, expexpr, eval expr)

main :: IO ()
main = doTests doEval [
    (x, Var "x"),
    (var, Var "var"),
    (true, true),
    (false, false),
    (zero, zero),
    (suc, suc),
    (nil, nil),
    (cons, cons),
    (App suc zero, one),
    (App suc (App suc zero), two),
    (App suc (App suc (App suc zero)), three),
    (App suc n, Con "Succ" [Var "n"]),
    (App suc (App suc n), Con "Succ" [Con "Succ" [Var "n"]]),
    (App cons zero, Con "Cons" [zero]),
    (App (Con "Cons" [zero]) nil, Con "Cons" [zero, nil]),
    (App (App cons zero) nil, Con "Cons" [zero, nil]),
    (App (App cons false) (App (App cons true) nil),
      Con "Cons" [false, Con "Cons" [true, nil]]),
    (App (App cons two) (App (App cons one) (App (App cons zero) nil)),
      Con "Cons" [two, Con "Cons" [one, Con "Cons" [zero, nil]]]),
    (App (App cons five) (App (App cons one) (App (App cons zero) xs)),
      Con "Cons" [five,
        Con "Cons" [one,
          Con "Cons" [zero,
            Var "xs"]]]),
    (App (Lam "x" x) zero, zero),
    (Let "x" zero x, zero),
    (Let "x" (Lam "y" y) (Let "z" nil (App x z)), nil),
    (Let "x" true (Case x [
        (Pat "False" [], true),
        (Pat "True" [], false)
      ]), false),
    (Let "x" (Lam "y" (Case y [
        (Pat "False" [], true),
        (Pat "True" [], false)
      ])) (Let "z" true (App x z)), false),
    (Let "iszero" (Lam "n" (Case n [
        (Pat "Zero" [], true),
        (Pat "Succ" ["m"], false)
      ])) (Let "x" two (App iszero x)), false),
    (Let "iszero" (Lam "n" (Case n [
        (Pat "Zero" [], true),
        (Pat "Succ" ["m"], false)
      ])) (Let "x" zero (App iszero x)), true),
    (Let "plus1" (Lam "n" (App suc n)) (Let "x" one (App plus1 x)), two),
    (Let "and" (Lam "n" (Lam "m" (Case n [
        (Pat "False" [], false),
        (Pat "True" [], m)
      ]))) (App (App (Var "and") true) true),
      true),
    (Let "pred" (Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ])) (App (Var "pred") zero),
      zero),
    (Let "pred" (Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ])) (App (Var "pred") two),
      one),
    (Let "plus" (Lam "n" (Lam "m" (Case n [
        (Pat "Zero" [], m),
        (Pat "Succ" ["n'"], App (App plus n') (App suc m))
      ]))) (App (App plus three) two),
      five),
    (Let "x" (Con "Zero" []) (App (Con "Succ" []) (Var "x")),
      Con "Succ" [Con "Zero" []])
  ]
  where
    x = Var "x"
    y = Var "y"
    z = Var "z"
    var = Var "var"
    n = Var "n"
    n' = Var "n'"
    m = Var "m"
    m' = Var "m'"
    xs = Var "xs"
    iszero = Var "iszero"
    plus1 = Var "plus1"
    plus = Var "plus"
    one = Con "Succ" [zero]
    two = Con "Succ" [one]
    three = Con "Succ" [two]
    four  = Con "Succ" [three]
    five  = Con "Succ" [four]
