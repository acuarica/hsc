
module Main where

import Test.HUnit

import Expr
import Parser

testCase :: (String, String, Expr) -> Test
testCase (message, code, expected) = TestCase (
    do
      putStrLn message
      assertEqual message (parseWith expr code) expected
  )

main :: IO Counts
main = runTestTT (TestList (map testCase [
    ("Var 1", "x", Var "x"),
    ("Lambda1", "{\\x->x}", Lam "x" (Var "x")),
    ("Let 1", "let x={\\y->y} in x", Let "x" (Lam "y" (Var "y")) (Var "x") )
  ]))
