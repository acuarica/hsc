
module Main where

import Expr
import Util

doFreeVars :: (Expr, [Var]) -> (String, [Var], [Var])
doFreeVars (expr, exp) = (show expr, exp, freeVars expr)

main :: IO ()
main = do
  doTests doFreeVars [
      (Var "x", ["x"])
    ]
  doTests doFreeVars [
      (Var "x", ["x"])
    ]
