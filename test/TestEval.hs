
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(..), Pat(Pat), con, app, zero, suc, cons, nil)
import Parser (parseExpr)
import Eval (eval, whnf)

whnfTest :: TestTree
whnfTest = testGroup "whnf" $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $
      (whnf . parseExpr) a @?= (whnf . parseExpr) e)
  [
    ("let x=(let y=A in y 0) in x y", "A 0 y")
  ]

evalTest :: TestTree
evalTest = testGroup "eval expr ~~> expr" $
  map (\(a, e) ->
    testCase (show a ++ " ~~> " ++ show e) $
      eval a @?= e)
  [
    (var, var),
    (con "True", con "True"),
    (cons, cons),
    (App suc zero, one),
    (App suc (App suc zero), two),
    (App suc (App suc (App suc zero)), three),
    (App suc n, Con "Succ" [n]),
    (App suc (App suc n), Con "Succ" [Con "Succ" [n]]),
    (App cons zero, Con "Cons" [zero]),
    (App (Con "Cons" [zero]) nil, Con "Cons" [zero, nil]),
    (app cons [zero, nil], Con "Cons" [zero, nil]),
    (app cons [x, app cons [y, z]], Con "Cons" [x, Con "Cons" [y, z]]),
    (App (App cons two) (App (App cons one) (App (App cons zero) nil)),
      Con "Cons" [two, Con "Cons" [one, Con "Cons" [zero, nil]]]),
    (App (App cons five) (App (App cons one) (App (App cons zero) xs)),
      Con "Cons" [five, Con "Cons" [one, Con "Cons" [zero, xs]]]),
    (App (Lam "x" x) zero, zero),
    (Lam "x" x, Lam "x" x),
    (Lam "f" (Lam "x" (App f x)), Lam "f" (Lam "x" (App f x))),
    (Let "x" zero x, zero),
    (Let "x" (Lam "y" y) (Let "z" nil (App x z)), nil),
    (Let "x" (con "True") (Case x [
        (Pat "False" [], con "True"),
        (Pat "True" [], con "False")
      ]), con "False"),
    (Let "x" (Lam "y" (Case y [
        (Pat "F" [], con "T"),
        (Pat "T" [], con "F")
      ])) (Let "z" (con "T") (App x z)), con "F"),
    (Let "iszero" (Lam "n" (Case n [
        (Pat "Zero" [], con "T"),
        (Pat "Succ" ["m"], con "F")
      ])) (Let "x" two (App iszero x)), con "F"),
    (Let "iszero" (Lam "n" (Case n [
        (Pat "Zero" [], con "T"),
        (Pat "Succ" ["m"], con "F")
      ])) (Let "x" zero (App iszero x)), con "T"),
    (Let "plus1" (Lam "n" (App suc n)) (Let "x" one (App plus1 x)), two),
    (Let "and" (Lam "n" (Lam "m" (Case n [
        (Pat "False" [], con "False"),
        (Pat "True" [], m)
      ]))) (app (Var "and") [con "True", con "True"]),
      con "True"),
    (Let "pred" (Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ])) (App (Var "pred") zero),
      zero),
    (Let "pred" (Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ])) (App (Var "pred") two),
      one),
    (Let "pl" (Lam "n" (Lam "m" (Case n [
        (Pat "Zero" [], m),
        (Pat "Succ" ["x"], App (App (Var "pl") x) (App suc m))
      ]))) (App (App (Var "pl") three) two),
      five),
    (Let "x" (Con "Zero" []) (App (Con "Succ" []) (Var "x")),
      Con "Succ" [Con "Zero" []]),
    (app cons [App f y, app g [f, ys]],
      Con "Cons" [App f y,app g [f, ys]]),
    (Case xs [], Case xs []),
    (Case xs [(Pat "A" [], zero), (Pat "B" [], one)],
      Case xs [(Pat "A" [], zero), (Pat "B" [], one)]),
    (Case xs [(Pat "A" ["x"], App suc x), (Pat "B" ["y"], App suc y)],
      Case xs [(Pat "A" ["x"], App suc x), (Pat "B" ["y"], App suc y)]),
    (Case (App f xs) [(Pat "A" [], zero)],
      Case (App f xs) [(Pat "A" [], zero)]),
    (Case (App suc (App f n)) [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], App f n')
      ], App f (App f n))
  ]
  where
    x = Var "x"
    y = Var "y"
    z = Var "z"
    var = Var "var"
    n = Var "n"
    n' = Var "n'"
    m = Var "m"
    m' = Var "m'"
    f = Var "f"
    g = Var "g"
    xs = Var "xs"
    ys = Var "ys"
    iszero = Var "iszero"
    plus1 = Var "plus1"
    plus = Var "plus"
    one = Con "Succ" [zero]
    two = Con "Succ" [one]
    three = Con "Succ" [two]
    four  = Con "Succ" [three]
    five  = Con "Succ" [four]

evalWithParseTest = testGroup "evalWithParse:eval . parseExpr" $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $
      (eval . parseExpr) a @?= (eval . parseExpr) e)
  [
    ("let x=(let y=Succ in y 0) in y", "y"),
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
    ("let fst={a->{b->a}} in let p=fst T in let a={f->{x->f x}} in a p F", "T"),
    ("let id={a-> a} in let app={f->{x->f x}} in app id [1,2,3,4]", "[1,2,3,4]"),
    ("let cp={a->case a of Zero->0;Succ b->Succ (cp b);} in cp 4", "4"),
    ("let fst={t->case t of Tup x y->x;} in fst (Tup 1 2)", "1"),
    ("let x=A B in Tup x x", "Tup (A B) (A B)"),
    ("(let x=F in x) (let x=X in x)", "F X")
  ] 

evalWithPreludeTest :: TestTree
evalWithPreludeTest = testGroup ("evalPreludeTest") $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $
      (eval . parseExpr . (++) prelude) a @?= (eval . parseExpr) e)
  [
    ("x", "x"), 
    ("cp 5", "5"),
    ("{x->x} (cp 5)", "5"),
    ("len [1,2,3,4,5,6,7]", "7"),
    ("plus 2 3", "5"),
    ("mult 4 5", "20"),
    ("cat [1,2,3,4] [5,6,7]", "[1,2,3,4,5,6,7]"),
    ("cat [] [One,Two,Three]", "[One,Two,Three]"),
    ("cat [One,Two,Three] []", "[One,Two,Three]"),
    ("cat (cat [1,2] [3]) [4,5,6]", "[1,2,3,4,5,6]"),
    ("rev [A,B,C,D]", "[D,C,B,A]"),
    ("rev []", "[]"),
    ("rev [One]", "[One]"),
    ("let reverse={rs->revA rs []} in reverse [A,B,C,D,E,F]", "[F,E,D,C,B,A]"),
    ("revA [A,B,C,D,E,F,G] []", "[G,F,E,D,C,B,A]"),
    ("map id [A,B,C,D,E]", "[A,B,C,D,E]"),
    ("rev (map rev [[A,B,C], [D,E], [F]])", "[[F],[E,D],[C,B,A]]"),
    ("let plus2={n -> plus 2 n} in plus2 1", "3"),
    ("let multten=mult 10 in multten 1", "10"),
    ("map {a->A} [1,2,3,4,5]", "[A,A,A,A,A]"),
    ("map {a->a} [1,2,3,4,5]", "[1,2,3,4,5]"),
    ("map {a->Succ (Succ a)} [1]", "[3]"),
    ("map {a->Succ (Succ a)} [1,2,3,4,5]", "[3,4,5,6,7]"),
    ("map {q->plus 10 q} [1,2,3,4,5]", "[11,12,13,14,15]"),
    ("let plusten={q->plus 10 q} in map plusten [1,2,3,4,5]", "[11,12,13,14,15]"),
    ("let two={a->{b->a}} in let plusten=two 10 in app plusten 10", "10"),
    ("{x->x} (plus 1) 1", "2"),
    ("{f->{x->f x}} (plus 1) 1", "2"),
    ("plus 0 1", "1"),
    ("let f={a->{b->case a of F-> a;T->Succ b;}} in f F T", "F"),
    ("plus 4 3", "7"),
    ("({f->{x->f x}} (plus 1)) 1", "2"),
    ("let plustwo=plus 2 in map plustwo [1,2,3,4,5]", "[3,4,5,6,7]"),
    ("let multtwo=mult 2 in map multtwo [1,2,3,4,5]", "[2,4,6,8,10]"),
    ("let multtwo=mult 2 in app multtwo 1", "2")
  ]
  where
    prelude =
      "let id={a->a} in \
      \let app={p->{q->p q}} in \
      \let inc={n->Succ n} in \
      \let cp={a->case a of Zero->0;Succ aa->Succ (cp aa);} in \
      \let cat={xs->{ys->case xs of Nil->ys;Cons z zs->Cons z (cat zs ys);}} in \
      \let rev={rs-> case rs of Nil->Nil;Cons s ss->cat (rev ss) [s];} in \
      \let revA={xs->{as->case xs of Nil->as;Cons y ys->revA ys (Cons y as);}}in \
      \let map={f->{xs->case xs of Nil->Nil;Cons y ys->Cons (f y)(map f ys);}}in \
      \let plus={n->{m->case n of Zero->m; Succ nn->plus nn (Succ m);}} in \
      \let mult={n->{m->case n of Zero->0; Succ nn->plus (mult nn m) m;}} in \
      \let len={xs->case xs of Nil->0; Cons y ys->Succ (len ys);} in "

evalLazyTest :: TestTree
evalLazyTest = testGroup ("eval lazy with prelude") $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $
      (eval . parseExpr . (++) prelude) a @?= (eval . parseExpr) e)
  [
    ("head infA", "A"),
    ("head (inf 1)", "1"),
    ("head (tail (tail (tail (inf 1))))", "4")
  ]
  where
    prelude = 
      "let head={xs->case xs of Cons y ys -> y; } in \
      \let tail={xs->case xs of Cons y ys -> ys; } in \
      \let inf={n->Cons n (inf (Succ n))} in \
      \let infA=Cons A inf in "

evalNameCaptureTest = testGroup "eval name capture: eval . parseExpr" $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $
      (eval . parseExpr) a @?= (eval . parseExpr) e)
  [
    ("let x=(let y=A in y 0) in x y", "A 0 y"),
    ("let x=(let y=A in let z=B in C y z) in x y z", "C A B y z"),
    ("(let x=A in C x) x", "C A x"),
    ("(let y=A in let z=B in C y z) y z", "C A B y z")
  ]

evalForwardDecl = testGroup "eval w/forward declarations" $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $
      (eval . parseExpr) a @?= (eval . parseExpr) e)
  [
    ("let a=b in let b=B in a", "B")
  ]

main :: IO ()
main = defaultMain $ testGroup "Eval::eval/whnf" $
  [whnfTest, evalTest, evalWithParseTest, evalWithPreludeTest, evalLazyTest, 
  evalNameCaptureTest, evalForwardDecl]
