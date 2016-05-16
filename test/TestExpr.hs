
module Main where

import Control.Arrow (first)

import Expr
import Parser
import Util

main :: IO ()
main = do
  doTests (first (freeVars . parse)) [
      ("x", ["x"]),
      ("f x", ["f", "x"]),
      ("f (g x) (h x y)", ["f", "g", "x", "h", "y"]),
      ("let inc={n->S n} in inc m", ["m"]),
      ("let inc={n->S n} in inc n", ["n"]),
      ("let inc={n->S m} in inc n", ["n", "m"])
    ]
  doTests (\(f, x, exp)->(app (parse f) (map parse x), parse exp)) [
      ("f", ["x", "y"], "f x y"),
      ("f", ["g x", "h y", "i (f x) y z"], "f (g x) (h y) (i (f x) y z)")
    ]
  where parse = parseExpr
