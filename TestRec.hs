
module Main where

import System.Exit
import Test.HUnit

import Util
import Expr
import Parser

doEval :: (String, Expr, Expr) -> (String, Expr, Expr)
doEval (s, expr, expexpr) = (s, eval expexpr, eval expr)

doParse :: (String, String) -> (String, Expr, Expr)
doParse (e, expexpr) = (e, parseExpr e, parseExpr expexpr)

main :: IO ()
main = (doTests doTest . map (doEval . doParse)) [
    ("let $x=0 in Succ $x", "1"),
    ("let $x=True in [$x]", "[True]"),
    ("let $id={\\$a-> $a} in $id [1,2,3,4,5]", "[1,2,3,4,5]"),
    ("let $two={\\$a->{\\$b->$a}} in $two True False", "True"),
    ("{\\$a->{\\$b->$a}}", "{\\$a->{\\$b->$a}}"),
    ("{\\$b->{\\$a->$b} A} B", "B"),
    ("{\\$a->{\\$a->$a} A} B", "A"),
    ("{\\$x->$x} {\\$a->{\\$b->$a}}", "{\\$a->{\\$b->$a}}"),
    ("let $a={\\$f->{\\$x->$f $x}} in $a", "{\\$f->{\\$x->$f $x}}"),
    ("{\\$f->{\\$x->$f $x}} {\\$b->T} F", "T"),
    ("let $a={\\$f->{\\$x->$f $x}} in $a {\\$b->T} F", "T"),
    ("{\\$a->Succ (Succ $a)} 1", "3"),
    ("let $sumtwo={\\$a->Succ (Succ $a)} in $sumtwo 1", "3"),
    ("let $a={\\$f->{\\$x->$f $x}} in $a {\\$n->Succ $n} 0", "1"),
    (
    "   let $two = {\\$a->{\\$b->$a}} \
    \in let $id  = {\\$c->$c} \
    \in $id $two", "{\\$a->{\\$b->$a}}"),
    (
    "   let $two = {\\$a->{\\$b->$a}} \
    \in let $id  = {\\$b->$b} \
    \in $id $two", "{\\$a->{\\$b->$a}}"),
    (
    "   let $two = {\\$a->{\\$b->$a}} \
    \in let $id  = {\\$a->$a} \
    \in $id $two", "{\\$a->{\\$b->$a}}"),
    (
    "   let $two = {\\$a->{\\$b->$a}} \
    \in let $p   = $two True \
    \in let $app = {\\$f->{\\$x->$f $x}}\
    \in $app $p False", "True"),
    (
    "let $id={\\$a-> $a}\
    \in let $app={\\$f->{\\$x->$f $x}}\
    \in $app $id [1,2,3,4,5]", "[1,2,3,4,5]"),
    (
    "let $copy={\\$a->case $a of {\
    \  0->0;\
    \  Succ $aa -> Succ ($copy $aa); }}\
    \in $copy 4", "4"),
    (
    "let $copy={\\$a->case $a of {\
    \  0->0;\
    \  Succ $aa -> Succ ($copy $aa); }}\
    \in {\\$x->$x} ($copy 5) ", "5"),
    (
    "let $len={\\$xs->case $xs of {\
    \  Nil->0;\
    \  Cons $y $ys -> Succ ($len $ys);}}\
    \in $len [1,2,3,4,5,6,7]", "7"),
    (
    "let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $l -> $plus $l (Succ $m); }}}\
    \in $plus 2 3", "5"),
    (
    "let $len={\\$xs->case $xs of {\
    \  Nil->0;\
    \  Cons $y $ys -> Succ ($len $ys);}}\
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $l -> $plus $l (Succ $m); }}}\
    \in $len [1,2,3,4,5,6,7]", "7"),
    (
    "let $mult={\\$n->{\\$m->case $n of {\
    \  0->0;\
    \  Succ $nn -> $plus ($mult $nn $m) $m;}}}\
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $nn -> $plus $nn (Succ $m); }}}\
    \in $mult 4 5", "20"),
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
    \in $reverse [One]", "[One]"),
    (
    "let $revAccum = {\\$xs->{\\$as->case $xs of {\
    \  Nil -> $as;\
    \  Cons $y $ys -> $revAccum $ys (Cons $y $as); }}}\
    \in let $reverse={\\$rs-> $revAccum $rs []}\
    \in $reverse [A,B,C,D,E,F,G]", "[G,F,E,D,C,B,A]"),
    (
    "let $id={\\$a->$a}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map $id [A,B,C,D,E]", "[A,B,C,D,E]"),
    (
    "let $append={\\$xs->{\\$ys->case $xs of {\
    \  Nil->$ys;\
    \  Cons $z $zs -> Cons $z ($append $zs $ys) ; }}}\
    \in let $rev={\\$rs-> case $rs of {\
    \  Nil->Nil;\
    \  Cons $s $ss -> $append ($rev $ss) [$s] ; }}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $rev ($map $rev [[A,B,C], [D,E], [F]])", "[[F],[E,D],[C,B,A]]"),
    (
    "let $mult={\\$n->{\\$m->case $n of {\
    \  0->0;\
    \  Succ $nn -> $plus ($mult $nn $m) $m;}}}\
    \in let $multten={\\$n -> $plus 2 $n}\
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $nn -> $plus $nn (Succ $m); }}}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $multten 1", "3"),
    (
    "let $mult={\\$n->{\\$m->case $n of {\
    \  0->0;\
    \  Succ $nn -> $plus ($mult $nn $m) $m;}}}\
    \in let $multten=$mult 10 \
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $nn -> $plus $nn (Succ $m); }}}\
    \in $multten 1", "10"),
    (
    "let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map {\\$a->A} [1,2,3,4,5]", "[A,A,A,A,A]"),
    (
    "let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map {\\$a->$a} [1,2,3,4,5]", "[1,2,3,4,5]"),
    (
    "let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map {\\$a->Succ (Succ $a)} [1]", "[3]"),
    (
    "let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map {\\$a->Succ (Succ $a)} [1,2,3,4,5]", "[3,4,5,6,7]"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map {\\$q->$plus 10 $q} [1,2,3,4,5]", "[11,12,13,14,15]"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in let $plusten={\\$q->$plus 10 $q}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map $plusten [1,2,3,4,5]", "[11,12,13,14,15]"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in let $two={\\$a->{\\$b->$a}}\
    \in let $plusten=$two 10 \
    \in let $app={\\$p->{\\$q->$p $q}}\
    \in $app $plusten 10", "10"),
    (
    "let $two={\\$a->{\\$b->$a}}\
    \in {\\$x->$x} ($two A) ", "{\\$b->A}"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> (Succ $b); }}}\
    \in {\\$x->$x} ($plus 1) 1", "2"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> (Succ $b); }}}\
    \in {\\$x->{\\$y->$x $y}} ($plus 1) 1", "2"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in $plus 0", "{\\$b->$b}"),
    (
    "let $f={\\$a->{\\$b->case $a of {\
    \  False -> $a ;\
    \  True  -> Succ $b; }}}\
    \in $f False", "{\\$b->False}"),
    (
    "let $f={\\$a->{\\$b->case $a of {\
    \  0 -> $a ;\
    \  Succ $aa  -> $aa ; }}}\
    \in $f 4", "{\\$b->3}"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> Succ ($plus $aa $b); }}}\
    \in $plus 4", "{\\$b->Succ (Succ (Succ (Succ $b)))}"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in $plus 4", "{\\$b->Succ (Succ (Succ (Succ $b)))}"),
    (
    "let $plus={\\$a->{\\$b->case $a of {\
    \  0->$b;\
    \  Succ $aa -> $plus $aa (Succ $b); }}}\
    \in ({\\$x->{\\$y->$x $y}} ($plus 1)) 1", "2"),
    (
    "let $plustwo=$plus 2 \
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $nn -> $plus $nn (Succ $m); }}}\
    \in let $map={\\$f->{\\$xs-> case $xs of {\
    \  Nil->Nil;\
    \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    \in $map $plustwo [1,2,3,4,5]", "[3,4,5,6,7]"),
    (
    "let $mult={\\$n->{\\$m->case $n of {\
    \  0->0;\
    \  Succ $nn -> $plus ($mult $nn $m) $m;}}}\
    \in let $multtwo=$mult 2 \
    \in let $plus={\\$n->{\\$m->case $n of {\
    \  0->$m;\
    \  Succ $nn -> $plus $nn (Succ $m); }}}\
    \in let $app={\\$f->{\\$x->$f $x}}\
    \in $app $multtwo 1", "3"),

    -- (
    -- "let $mult={\\$n->{\\$m->case $n of {\
    -- \  0->0;\
    -- \  Succ $nn -> $plus ($mult $nn $m) $m;}}}\
    -- \in let $multtwo=$mult 2 \
    -- \in let $plus={\\$n->{\\$m->case $n of {\
    -- \  0->$m;\
    -- \  Succ $nn -> $plus $nn (Succ $m); }}}\
    -- \in let $map={\\$f->{\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    -- \in let $app={\\$f->{\\$x-> $f $x}}\
    -- \in $app $multtwo 1", "2"),

    -- (
    -- "let $map={\\$f->{\\$xs-> case $xs of {\
    -- \  Nil->Nil;\
    -- \  Cons $y $ys -> Cons ($f $y) ($map $f $ys) ; }}}\
    -- \in $map ({\\$x->X})", "{\\$b->Succ (Succ (Succ (Succ $b)))}"),

    ("$x", "$x")
  ]
