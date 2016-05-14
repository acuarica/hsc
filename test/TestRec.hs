
module Main where

import Util
import Expr
import Parser
import Eval

doEval :: (String, Expr, Expr) -> (String, Expr, Expr)
doEval (s, expr, expexpr) = (s, eval expexpr, eval expr)

doParse :: (String, String) -> (String, Expr, Expr)
doParse (act, expexpr) = (act, parseExpr act, parseExpr expexpr)

e = parseExpr "let x=(let cons=Cons in cons 0) in x cons"

--a = let b = (let c=1 in c) in b+c

main :: IO ()
main = doTests (doEval . doParse) [
    --("let x=(let y=Succ in y 0) in y", "A"),
    ("let x=0 in Succ x", "1"),
    ("let x=True in [x]", "[True]"),
    ("let id={a->a} in id [1,2,3,4,5]", "[1,2,3,4,5]"),
    ("let two={a->{b->a}} in two True False", "True"),
    ("{a->{b->a}}", "{a->{b->a}}"),
    ("{b->{a->b} A} B", "B"),
    ("{a->{x->x} A} B", "A"),
    ("{x->x} {a->{b->a}}", "{a->{b->a}}"),
    ("{f->{x->f x}}", "{f->{x->f x}}"),
    ("let a={f->{x->f x}} in a", "{f->{x->f x}}"),
    ("{f->{x->f x}} {b->T} F", "T"),
    (
    "let head={xs->case xs of Cons y ys -> y; } \
    \in let inf=Cons A inf in head inf", "A"),
    (
    "let head={xs->case xs of Cons y ys -> y; } \
    \in let inf={n->Cons n (inf (Succ n))} in head (inf 1)", "1"),
    (
    "let head={xs->case xs of Cons y ys -> y; } \
    \ in let tail={xs->case xs of Cons y ys -> ys; } \
    \ in let inf={n->Cons n (inf (Succ n))} \
    \ in head (tail (tail (tail (inf 1))))", "4"),
    ("let a={f->{x->f x}} in a {b->T} F", "T"),
    ("let a={f->{x->f x}} in a {b->T}", "{x->{b->T} x}"),
    ("let a={f->{x->f x}} in a {b->Succ b}", "{x->{b->Succ b} x}"),
    ("{a->Succ (Succ a)} 1", "3"),
    ("let sumtwo={a->Succ (Succ a)} in sumtwo 1", "3"),
    ("let a={f->{x->f x}} in a {n->Succ n} 0", "1"),
    ("let two={a->{b->a}} in {x->x} (two A) ", "{b->A}"),
    ("let two={a->{b->a}} in let id={c->c} in id two", "{a->{b->a}}"),
    ("let fst={a->{b->a}} in let id={b->b} in id fst", "{a->{b->a}}"),
    ("let fst={a->{b->a}} in let id={a->a} in id fst", "{a->{b->a}}"),
    (
    "let fst={a->{b->a}} \
    \in let p=fst True \
    \in let app={f->{x->f x}}\
    \in app p False", "True"),
    (
    "let id={a-> a}\
    \in let app={f->{x->f x}}\
    \in app id [1,2,3,4,5]", "[1,2,3,4,5]"),
    ("let cp={a->case a of Zero->0; Succ b->Succ (cp b); } in cp 4", "4"),
    (
    "let cp={a->case a of \
    \  Zero->0;\
    \  Succ aa -> Succ (cp aa); }\
    \in {x->x} (cp 5)", "5"),
    ("{f->{x->f x}}", "{f->{x->f x}}"),
    (
    "let cp={a->case a of Zero->0; Succ aa->Succ (cp aa);} \
    \in {f->{x->f x}}", "{f->{x->f x}}"),
    (
    "let cp={a->case a of Zero->0; Succ aa->Succ (cp aa);} \
    \in cp 5", "5"),
    (
    "let len={xs->case xs of \
    \  Nil->0;\
    \  Cons y ys -> Succ (len ys);}\
    \in len [1,2,3,4,5,6,7]", "7"),
    (
    "let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ l -> plus l (Succ m); }}\
    \in plus 2 3", "5"),
    (
    "let len={xs->case xs of \
    \  Nil->0;\
    \  Cons y ys -> Succ (len ys);}\
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ l -> plus l (Succ m); }}\
    \in len [1,2,3,4,5,6,7]", "7"),
    (
    "let mult={n->{m->case n of \
    \  Zero->0;\
    \  Succ nn -> plus (mult nn m) m;}}\
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ nn -> plus nn (Succ m); }}\
    \in mult 4 5", "20"),
    (
    "let append={xs->{ys->case xs of \
     \  Nil->ys;\
     \  Cons z zs -> Cons z (append zs ys) ; }}\
     \in append [1,2,3,4] [5,6,7]", "[1,2,3,4,5,6,7]"),
    (
    "let append={xs->{ys->case xs of \
    \  Nil->ys;\
    \  Cons z zs -> Cons z (append zs ys) ; }}\
    \in append [] [One,Two,Three]", "[One,Two,Three]"),
    (
    "let append={xs->{ys->case xs of \
    \  Nil->ys;\
    \  Cons z zs -> Cons z (append zs ys) ; }}\
    \in append [One,Two,Three] []", "[One,Two,Three]"),
    (
    "let cat={xs->{ys->case xs of\
    \  Nil->ys; Cons z zs->Cons z (cat zs ys); }}\
    \in let rev={rs->case rs of\
    \  Nil->Nil; Cons s ss->cat (rev ss) [s]; }\
    \in rev [A,B,C,D]", "[D,C,B,A]"),
    (
    "let cat={xs->{ys->case xs of \
    \  Nil->ys;\
    \  Cons z zs -> Cons z (cat zs ys) ; }}\
    \in let rev={rs-> case rs of \
    \  Nil->Nil;\
    \  Cons s ss -> cat (rev ss) [s] ; }\
    \in rev []", "[]"),
    (
    "let cat={xs->{ys->case xs of \
    \  Nil->ys;\
    \  Cons z zs -> Cons z (cat zs ys) ; }}\
    \in let rev={rs-> case rs of \
    \  Nil->Nil;\
    \  Cons s ss -> cat (rev ss) [s] ; }\
    \in rev [One]", "[One]"),
    (
    "let revAccum={xs->{as->case xs of \
    \  Nil -> as;\
    \  Cons y ys -> revAccum ys (Cons y as); }}\
    \in let reverse={rs->revAccum rs []}\
    \in reverse [A,B,C,D,E,F,G]", "[G,F,E,D,C,B,A]"),
    (
    "let revAccum={xs->{as->case xs of \
    \  Nil -> as;\
    \  Cons y ys -> revAccum ys (Cons y as); }}\
    \in revAccum [A,B,C,D,E,F,G] []", "[G,F,E,D,C,B,A]"),
    (
    "let id={a->a}\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in map id [A,B,C,D,E]", "[A,B,C,D,E]"),
    (
    "let append={xs->{ys->case xs of \
    \  Nil->ys;\
    \  Cons z zs -> Cons z (append zs ys) ; }}\
    \in let rev={rs-> case rs of \
    \  Nil->Nil;\
    \  Cons s ss -> append (rev ss) [s] ; }\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in rev (map rev [[A,B,C], [D,E], [F]])", "[[F],[E,D],[C,B,A]]"),
    (
    "let mult={n->{m->case n of \
    \  Zero->0;\
    \  Succ nn -> plus (mult nn m) m;}}\
    \in let multten={n -> plus 2 n}\
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ nn -> plus nn (Succ m); }}\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in multten 1", "3"),
    (
    "let mult={n->{m->case n of \
    \  Zero->0;\
    \  Succ nn -> plus (mult nn m) m;}}\
    \in let multten=mult 10 \
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ nn -> plus nn (Succ m); }}\
    \in multten 1", "10"),
    (
    "let map={f->{xs->case xs of \
    \  Nil->Nil; \
    \  Cons y ys->Cons (f y) (map f ys);}}\
    \in map {a->A} [1,2,3,4,5]", "[A,A,A,A,A]"),
    (
    "let map={f->{xs-> case xs of \
    \  Nil->Nil; \
    \  Cons y ys -> Cons (f y) (map f ys);}}\
    \in map {a->a} [1,2,3,4,5]", "[1,2,3,4,5]"),
    (
    "let map={f->{xs-> case xs of \
    \  Nil->Nil; \
    \  Cons y ys -> Cons (f y) (map f ys); }}\
    \in map {a->Succ (Succ a)} [1]", "[3]"),
    (
    "let map={f->{xs-> case xs of \
    \  Nil->Nil; \
    \  Cons y ys -> Cons (f y) (map f ys); }}\
    \in map {a->Succ (Succ a)} [1,2,3,4,5]", "[3,4,5,6,7]"),
    (
    "let plus={a->{b->case a of \
    \  Zero->b;\
    \  Succ aa -> plus aa (Succ b); }}\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in map {q->plus 10 q} [1,2,3,4,5]", "[11,12,13,14,15]"),
    (
    "let plus={a->{b->case a of \
    \  Zero->b;\
    \  Succ aa -> plus aa (Succ b); }}\
    \in let plusten={q->plus 10 q}\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in map plusten [1,2,3,4,5]", "[11,12,13,14,15]"),
    (
    "let plus={a->{b->case a of \
    \  Zero->b;\
    \  Succ aa -> plus aa (Succ b); }}\
    \in let two={a->{b->a}}\
    \in let plusten=two 10 \
    \in let app={p->{q->p q}}\
    \in app plusten 10", "10"),
    (
    "let plus={a->{b->case a of Zero->b;Succ aa -> (Succ b); }}\
    \in {x->x} (plus 1) 1", "2"),
    (
    "let plus={a->{b->case a of \
    \  Zero->b;\
    \  Succ aa -> (Succ b); }}\
    \in {f->{x->f x}} (plus 1) 1", "2"),
    (
    "let plus={a->{b->case a of \
    \  Zero->b;\
    \  Succ aa -> plus aa (Succ b); }}\
    \in plus 0 1", "1"),
    (
    "let f={a->{b->case a of \
    \  False -> a ;\
    \  True  -> Succ b; }}\
    \in f False True", "False"),
    (
    "let plus={a->{b->case b of \
    \  Zero->a;\
    \  Succ bb -> plus (Succ a) bb; }}\
    \in plus 4 3", "7"),
    (
    "let plus={a->{b->case a of \
    \  Zero->b;\
    \  Succ aa -> plus aa (Succ b); }}\
    \in ({f->{x->f x}} (plus 1)) 1", "2"),
    (
    "let plustwo=plus 2 \
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ nn -> plus nn (Succ m); }}\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in map plustwo [1,2,3,4,5]", "[3,4,5,6,7]"),
    (
    "let mult={p->{q->case p of \
    \  Zero->0;\
    \  Succ pp -> plus (mult pp q) q;}}\
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ nn -> plus nn (Succ m); }}\
    \in let multtwo=mult 2 \
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in map multtwo [1,2,3,4,5]", "[2,4,6,8,10]"),
    (
    "let mult={n->{m->case n of \
    \  Zero->0;\
    \  Succ nn -> plus (mult nn m) m;}}\
    \in let multtwo=mult 2 \
    \in let plus={n->{m->case n of \
    \  Zero->m;\
    \  Succ nn -> plus nn (Succ m); }}\
    \in let map={f->{xs-> case xs of \
    \  Nil->Nil;\
    \  Cons y ys -> Cons (f y) (map f ys) ; }}\
    \in let app={f->{x-> f x}}\
    \in app multtwo 1", "2")
    -- (
    -- "let map={f->{xs->case xs of \
    -- \  Nil->Nil; \
    -- \  Cons y ys-> Cons (f y) (map f ys);}} \
    -- \in map g (map f ys)", "x")
  ]
