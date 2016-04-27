
module Main where

import System.Exit
import Test.HUnit

import Expr
import Parser

doPrint :: (Expr, Expr, Expr) -> String
doPrint (expr, expexpr, actexpr) =
  pprint expr ++ " ~~> " ++ pprint actexpr

doTest :: (String, Expr, Expr) -> Test
doTest (expr, expexpr, actexpr) = TestCase (
    assertEqual expr expexpr actexpr
  )

doTests :: [(String, Expr, Expr)] -> IO ()
doTests tests = do
  --mapM_ (putStrLn . doPrint) tests
  counts <- runTestTT (TestList (map doTest tests))
  exitWith (if errors counts > 0 || failures counts > 0
    then ExitFailure (errors counts + failures counts)
    else ExitSuccess)

doEval :: (String, Expr, Expr) -> (String, Expr, Expr)
doEval (s, expr, expexpr) = (s, expexpr, eval expr)

doParse :: (String, String) -> (String, Expr, Expr)
doParse (e, expexpr) = (e, parseExpr e, parseExpr expexpr)

main :: IO ()
main = (doTests . map (doEval . doParse)) [
    ("let $x=0 in Succ $x", "1"),
    ("let $x=True in [$x]", "[True]"),
    (
    "let $len={\\$xs->case $xs of {\
    \  Nil->0;\
    \  Cons $y $ys -> $plus 1 ($len $ys);}}\
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $l -> $plus $l (Succ $m); }}}\
    \in $len [1,2,3,4,5,6,7]", "7"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
     \  Nil->$ys;\
     \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
     \in $append [1,2,3,4] [5,6,7]", "[1,2,3,4,5,6,7]"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in $append [] [One,Two,Three]", "[One,Two,Three]"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in $append [One,Two,Three] []", "[One,Two,Three]"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in let $reverse={\\$rs-> case $rs of {\
    \  Nil->Nil;\
    \  Cons $s $ss -> $append ($reverse $ss) [$s] ; }}\
    \in $reverse [One,Two,Three,Four]", "[Four,Three,Two,One]"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in let $reverse={\\$rs-> case $rs of {\
    \  Nil->Nil;\
    \  Cons $s $ss -> $append ($reverse $ss) [$s] ; }}\
    \in $reverse []", "[]"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in let $reverse={\\$rs-> case $rs of {\
    \  Nil->Nil;\
    \  Cons $s $ss -> $append ($reverse $ss) [$s] ; }}\
    \in $reverse [One]", "[One]")
  ]
