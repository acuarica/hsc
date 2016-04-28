
module Main where

import Util
import Expr
import Parser

doSuper :: (String, String) -> (String, Expr, Expr)
doSuper (act, exp) = (act, parseExpr exp, (supercompile . parseExpr) act)

main :: IO ()
main = mapM_ (print . supercompile . parseExpr . fst) [
    ("$x", "$x"),
    ("$var", "$var"),
    ("{\\$x->$x $x}", ""),
    (
    "let $mult={\\$n->{\\$m->case $n of {\
    \  0->0;\
    \  Succ $nn -> $plus ($mult $nn $m) $m;}}}\
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $nn -> $plus $nn (Succ $m); }}}\
    \in let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in let $rev={\\$rs-> case $rs of {\
    \  Nil->Nil;\
    \  Cons $s $ss -> $append ($rev $ss) [$s] ; }}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map ($ $rev [[A,B,C], [D,E], [F]])", "[[F],[E,D],[C,B,A]]")
  ]
