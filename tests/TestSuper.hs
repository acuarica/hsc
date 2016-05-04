
module Main where

import System.Exit
import Test.HUnit

import Util
import Expr
import Parser
import Super
import Eval
import Pretty

doSuper :: (String, Expr, Expr) -> (String, Expr, Expr)
doSuper (s, expr, expexpr) = (s, eval expexpr, eval expr)

doParse :: (String, String) -> (String, Expr, Expr)
doParse (e, expexpr) = (pretty (parseExpr e), parseExpr e, parseExpr expexpr)

main :: IO ()
main = (doTests doTest . map (doSuper . doParse)) [
    (
    "   let $inc={\\$n->Succ $n}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map $inc", "A")

    -- (
    -- "let $mapinc={\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
    -- \in $mapinc", "A"),
  ]
