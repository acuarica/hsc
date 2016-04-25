
module Main where

import Expr

es = [
  Con "True" [],
  Con "[]" [],
  Con ":" [],
  Con "Z" [],
  App (Con "S" []) (Con "Z" []),
  App (App (Con ":" []) (Con "Z" [])) (Con "[]" []),
  Lam "x" (Var "x"),
  App (Lam "x" (Var "x")) (Con "Z" []),
  Let "x" (Con "Z" []) (Var "x"),
  Let "x" (Lam "a" (Var "a"))
    (Let "y" (Con "[]" [])
      (App (Var "x") (Var "y"))),
  Let "x" (Con "True" [])
    (Case (Var "x") [
      (Con "False" [], Con "True" []),
      (Con "True" [], Con "False" [])
    ]),
  Let "x" (Lam "a" (Case (Var "a") [
    (Con "False" [], Con "True" []), (Con "True" [], Con "False" [])]
    ))
    (Let "y" (Con "True" [])
      (App (Var "x") (Var "y"))
    ),
  Let "iszero" (Lam "n" (Case (Var "n") [
        (Con "Z" [], Con "True" []),
        (App (Con "S" []) (Var "m"), Con "False" [])
      ]
    ))
    (Let "x" (App (Con "S" []) (App (Con "S" []) (Con "Z" [])))
      (App (Var "iszero") (Var "x"))
    ),
  Let "iszero" (Lam "n" (Case (Var "n") [
        (Con "Z" [], Con "True" []),
        (App (Con "S" []) (Var "m"), Con "False" [])
      ]
    ))
    (Let "x" (Con "Z" [])
      (App (Var "iszero") (Var "x"))
    )
  ]

doExpr :: Expr -> String
doExpr expr = pprint expr ++ " ~~> " ++ pprint (eval expr)

main :: IO [()]
main = mapM (putStrLn . doExpr) es
