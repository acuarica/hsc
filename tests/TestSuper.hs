
module Main where

import System.Exit
import Test.HUnit

import Util
import Expr
import Parser
--import Super
import Eval

super = eval
doSuper :: (String, Expr, Expr) -> (String, Expr, Expr)
doSuper (s, expr, expexpr) = (s, super expexpr, super expr)

doParse :: (String, String) -> (String, Expr, Expr)
doParse (e, expexpr) = (e, parseExpr e, parseExpr expexpr)

main :: IO ()
main = (doTests doTest . map (doSuper . doParse)) [
    (
    "   let $inc={\\$n->Succ $n}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map $inc", "A"),

    -- (
    -- "let $mapinc={\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons (Succ $y) ($mapinc $ys) ; }}\
    -- \in $mapinc", "A"),

    ("$x", "$x")
  ]
