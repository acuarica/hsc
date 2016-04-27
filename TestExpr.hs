
module Main where

import Test.HUnit

import Expr
import Parser

doExpr :: Expr -> String
doExpr expr =  pprint expr ++ " ~~> " ++ pprint (eval expr)
   ++ " | " ++ show expr ++ " ~~> " ++ show (eval expr)

testCase :: (Expr, Expr) -> Test
testCase (code, expected) = TestCase (
   assertEqual ("For " ++ show code) expected (eval code)
 )

x = Var "x"
var = Var "var"
n = Var "n"
true = Con "True" []
false = Con "False" []
zero = Con "Zero" []
suc = Con "Succ" []
one = Con "Succ" [zero]
two = Con "Succ" [one]
nil = Con "Nil" []
cons = Con "Cons" []

main :: IO Counts
main = runTestTT (TestList (map testCase [
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
  -- Lam "x" (Var "x"),
  -- App (Lam "x" (Var "x")) (Con "Z" []),
  -- Let "x" (Con "Z" []) (Var "x"),
  -- Let "x" (Lam "a" (Var "a"))
  --   (Let "y" (Con "[]" [])
  --     (App (Var "x") (Var "y"))),
  -- Let "x" (Con "True" [])
  --   (Case (Var "x") [
  --     (Con "False" [], Con "True" []),
  --     (Con "True" [], Con "False" [])
  --   ]),
  -- Let "x" (Lam "a" (Case (Var "a") [
  --   (Con "False" [], Con "True" []), (Con "True" [], Con "False" [])]
  --   ))
  --   (Let "y" (Con "True" [])
  --     (App (Var "x") (Var "y"))
  --   ),
  -- Let "iszero" (Lam "n" (Case (Var "n") [
  --       (Con "Z" [], Con "True" []),
  --       (App (Con "S" []) (Var "m"), Con "False" [])
  --     ]
  --   ))
  --   (Let "x" (App (Con "S" []) (App (Con "S" []) (Con "Z" [])))
  --     (App (Var "iszero") (Var "x"))
  --   ),
  -- Let "iszero" (Lam "n" (Case (Var "n") [
  --       (Con "Z" [], Con "True" []),
  --       (App (Con "S" []) (Var "m"), Con "False" [])
  --     ]
  --   ))
  --   (Let "x" (Con "Z" [])
  --     (App (Var "iszero") (Var "x"))
  --   ),
  -- Let "plus1" (Lam "n" (Case (Var "n") [
  --       (Con "Z" [], App (Con "S" []) (Con "Z" [])),
  --       (App (Con "S" []) (Var "m"), Con "False" [])
  --     ]
  --   ))
  --   (Let "x" (Con "Z" [])
  --     (App (Var "plus1") (Var "x"))
  --   ),
  -- Let "plus1" (Lam "n" (Case (Var "n") [
  --       (Con "Z" [], App (Con "S" []) (Con "Z" [])),
  --       (App (Con "S" []) (Var "m"), Var "m")
  --     ]
  --   ))
  --   (Let "x" (App (Con "S" []) (App (Con "S" []) (Con "Z" [])))
  --     (App (Var "plus1") (Var "x"))
  --   )
  (Var "x", Var "x")
  ]))
