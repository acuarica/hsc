
module Main where

import Expr
import Eval
import Pretty
import Util

doEval :: (Expr, Expr) -> (String, Expr, Expr)
doEval (expr, expexpr) = (show expr, expexpr, eval expr)

main :: IO ()
main = doTests doEval [
    (x, Var "x" True),
    (var, Var "var" True),
    (true, true),
    (false, false),
    (zero, zero),
    (suc, suc),
    (nil, nil),
    (cons, cons),
    (App suc zero, one),
    (App suc (App suc zero), two),
    (App suc (App suc (App suc zero)), three),
    (App suc n, Con "Succ" [Var "n" True]),
    (App suc (App suc n), Con "Succ" [Con "Succ" [Var "n" True]]),
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
            Var "xs" True]]]),
    (App (Lam "x" x) zero, zero),
    (Let "x" zero x, zero),
    (Let "x" (Lam "y" y) (Let "z" nil (App x z)), nil),
    (Let "x" true (Case x [
        (Pat "False" [], true),
        (Pat "True" [], false)
      ] False), false),
    (Let "x" (Lam "y" (Case y [
        (Pat "False" [], true),
        (Pat "True" [], false)
      ] False)) (Let "z" true (App x z)), false),
    (Let "iszero" (Lam "n" (Case n [
        (Pat "Zero" [], true),
        (Pat "Succ" ["m"], false)
      ] False)) (Let "x" two (App iszero x)), false),
    (Let "iszero" (Lam "n" (Case n [
        (Pat "Zero" [], true),
        (Pat "Succ" ["m"], false)]
      False )) (Let "x" zero (App iszero x)), true),
    (Let "plus1" (Lam "n" (App suc n)) (Let "x" one (App plus1 x)), two),
    (Let "and" (Lam "n" (Lam "m" (Case n [
        (Pat "False" [], false),
        (Pat "True" [], m)
      ] False))) (App (App (usevar "and") true) true),
      true),
    (Let "pred" (Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ] False)) (App (usevar "pred") zero),
      zero),
    (Let "pred" (Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ] False)) (App (usevar "pred") two),
      one),
    (Let "plus" (Lam "n" (Lam "m" (Case n [
        (Pat "Zero" [], m),
        (Pat "Succ" ["n'"], App (App plus n') (App suc m))
      ] False))) (App (App plus three) two),
      five),
    (Let "x" (Con "Zero" []) (App (Con "Succ" []) (usevar "x")),
      Con "Succ" [Con "Zero" []])
  ]
  where
    x = usevar "x"
    y = usevar "y"
    z = usevar "z"
    var = usevar "var"
    n = usevar "n"
    n' = usevar "n'"
    m = usevar "m"
    m' = usevar "m'"
    xs = usevar "xs"
    iszero = usevar "iszero"
    plus1 = usevar "plus1"
    plus = usevar "plus"
    one = Con "Succ" [zero]
    two = Con "Succ" [one]
    three = Con "Succ" [two]
    four  = Con "Succ" [three]
    five  = Con "Succ" [four]
