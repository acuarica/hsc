
module Main where

import System.Exit
import Test.HUnit

import Expr
import Parser

doPrint :: (Expr, Expr, Expr) -> String
doPrint (expr, expexpr, actexpr) =
  pprint expr ++ " ~~> " ++ pprint actexpr

doTest :: (Expr, Expr, Expr) -> Test
doTest (expr, expexpr, actexpr) = TestCase (
    assertEqual (show expr) expexpr actexpr
  )

doTests :: [(Expr, Expr, Expr)] -> IO ()
doTests tests = do
  mapM_ (putStrLn . doPrint) tests
  counts <- runTestTT (TestList (map doTest tests))
  exitWith (if errors counts > 0 || failures counts > 0
    then ExitFailure (errors counts + failures counts)
    else ExitSuccess)

doEval :: (Expr, Expr) -> (Expr, Expr, Expr)
doEval (expr, expexpr) = (expr, expexpr, eval expr)

main :: IO ()
main = (doTests . map doEval) [
    (x, x),
    (var, var),
    (true, true),
    (false, false),
    (zero, zero),
    (suc, suc),
    (nil, nil),
    (cons, cons),
    (App suc zero, one),
    (App suc n, Con "Succ" [n]),
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
      ]))) (App (App (Var "and") true) true),
      true),
    (Let "pred" (Lam "n" (Case n [
        (zero, zero),
        (App suc n', n')
      ])) (App (Var "pred") zero),
      zero),
    (Let "pred" (Lam "n" (Case n [
        (zero, zero),
        (App suc n', n')
      ])) (App (Var "pred") two),
      one),
    (Let "plus" (Lam "n" (Lam "m" (Case n [
        (zero, m),
        (App suc n', App (App plus n') (App suc m))
      ]))) (App (App plus three) two),
      five)
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
    iszero = Var "iszero"
    plus1 = Var "plus1"
    plus = Var "plus"
    true = Con "True" []
    false = Con "False" []
    zero = Con "Zero" []
    suc = Con "Succ" []
    one = Con "Succ" [zero]
    two = Con "Succ" [one]
    three = Con "Succ" [two]
    four = Con "Succ" [three]
    five = Con "Succ" [four]
    nil = Con "Nil" []
    cons = Con "Cons" []
