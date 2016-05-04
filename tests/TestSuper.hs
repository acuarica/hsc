
module Main where

import System.Exit
import Test.HUnit

import Util
import Expr
import Parser

doSuper :: (String, Expr, Expr) -> (String, Expr, Expr)
doSuper (s, expr, expexpr) = (s, eval expexpr, eval expr)

doParse :: (String, String) -> (String, Expr, Expr)
doParse (e, expexpr) = (e, parseExpr e, parseExpr expexpr)

main :: IO ()
main = (doTests doTest . map (doSuper . doParse)) [
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in let $plusten={\\$q->$plus 10 $q}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map $plusten", "A"),

    ("$x", "$x")
  ]
