
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Expr (Expr(Var, Con, Lam, App, Let, Case), Pat(Pat),
  con, app, let1, zero, suc, cons, nil, alpha)
import Parser (parseExpr)
import Eval (eval, whnf, evalc, whnfc)

evalp :: String -> Expr
evalp = alpha . eval . parseExpr

whnfp :: String -> Expr
whnfp = whnf . parseExpr

prelude :: String
prelude =
  "let \
  \  id = {a->a} ;\
  \  app = {p->{q->p q}} ;\
  \  inc = {n->Succ n} ;\
  \  copyN = {n->case n of \
  \    Zero -> 0;\
  \    Succ n' -> Succ (copyN n');\
  \  };\
  \  append = {xs->{ys->case xs of \
  \    Nil -> ys;\
  \    Cons x xs' -> Cons x (append xs' ys);\
  \  }};\
  \  reverse' = {xs->case xs of \
  \    Nil -> Nil;\
  \    Cons x xs' -> append (reverse' xs') [x];\
  \  };\
  \  reverse = {xs-> \
  \    let reverseAccum = {xs->{as->case xs of \
  \      Nil -> as;\
  \      Cons y ys -> reverseAccum ys (Cons y as);\
  \    }} in reverseAccum xs [] }  ;\
  \  map = {f->{xs->case xs of \
  \    Nil->Nil;\
  \    Cons y ys->Cons (f y)(map f ys);\
  \  }};\
  \  plus = {n->{m->case n of \
  \    Zero->m;\
  \    Succ nn->plus nn (Succ m);\
  \  }};\
  \  mult = {n->{m->case n of \
  \    Zero->0;\
  \    Succ nn->plus (mult nn m) m;\
  \  }};\
  \  len = {xs->case xs of Nil->0; Cons y ys->Succ (len ys);} ; \
  \  head = {xs->case xs of Cons y ys -> y; } ; \
  \  tail = {xs->case xs of Cons y ys -> ys; } ; \
  \  inf = {n->Cons n (inf (Succ n))} ; \
  \  infA = Cons A inf in "

whnfTest :: TestTree
whnfTest = testGroup "whnf" $
  let go a e = testCase (a ++ " ~~> " ++ e) $ whnfp a @?= whnfp e in
  [
    go "let x=(let y=A in y 0) in x y" "A 0 y",
    go "let x=y in let y=A in x" "A"
  ]

whnfcTest :: TestTree
whnfcTest = testGroup "whnfc" $
  let go a e c =
        let (we, steps) = (whnfc . parseExpr) a in
          testCase (a ++ " ~~>(" ++ show steps ++ ") " ++ e) $
            (we, steps) @?= (whnfp e, c) in
  [
    go "var" "var" 0,
    go "A" "A" 0,
    go "Succ var" "Succ var" 2,
    go "Succ (A B)" "Succ (A B)" 2,
    go "Cons A Nil" "[A]" 4,
    go "Branch A Nil Nil" "Branch A Nil Nil" 6,
    go "{x->x}" "{x->x}" 0,
    go "case True of False->A; True->B;" "B" 2,
    go "{x->x} A" "A" 2,
    go "let x=A in x" "A" 3,
    go "let x=A B in x C" "A B C" 7,
    go "let x=(let y=A in y 0) in x y" "A 0 y" 10
  ]

evalcTest :: TestTree
evalcTest = testGroup "evalc" $
  let go a e c =
        let (we, steps) = (evalc . parseExpr) a in
          testCase (a ++ " ~~>(" ++ show steps ++ ") " ++ e) $
            (we, steps) @?= (evalp e, c) in
  [
    go "A" "A" 0,
    go "Succ var" "Succ var" 2,
    go "Cons A Nil" "[A]" 4,
    go "Branch A Nil Nil" "Branch A Nil Nil" 6,
    go "Succ (A B)" "Succ (A B)" 4,
    go "Cons (A B) (Cons (C D) Nil)" "[A B, C D]" 12
  ]

evalTest :: TestTree
evalTest = testGroup "eval expr ~~> expr" $
  map (\(a, e) ->
    testCase (show a ++ " ~~> " ++ show e) $
      eval a @?= alpha e)
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
    (let1 "x" zero x, zero),
    (Let [("x", Lam "y" y)] (Let [("z", nil)] (App x z)), nil),
    (Let [("x", con "True")] (Case x [
        (Pat "False" [], con "True"),
        (Pat "True" [], con "False")
      ]), con "False"),
    (Let [("x", Lam "y" (Case y [
        (Pat "F" [], con "T"),
        (Pat "T" [], con "F")
      ]))] (Let [("z", con "T")] (App x z)), con "F"),
    (Let [("iszero", Lam "n" (Case n [
        (Pat "Zero" [], con "T"),
        (Pat "Succ" ["m"], con "F")
      ]))] (Let [("x", two)] (App iszero x)), con "F"),
    (Let [("iszero", Lam "n" (Case n [
        (Pat "Zero" [], con "T"),
        (Pat "Succ" ["m"], con "F")
      ]))] (Let [("x", zero)] (App iszero x)), con "T"),
    (Let [("plus1", Lam "n" (App suc n))]
      (Let [("x", one)] (App plus1 x)), two),
    (Let [("and", Lam "n" (Lam "m" (Case n [
        (Pat "False" [], con "False"),
        (Pat "True" [], m)
      ])))] (app (Var "and") [con "True", con "True"]),
      con "True"),
    (Let [("pred", Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ]))] (App (Var "pred") zero),
      zero),
    (Let [("pred", Lam "n" (Case n [
        (Pat "Zero" [], zero),
        (Pat "Succ" ["n'"], n')
      ]))] (App (Var "pred") two),
      one),
    (Let [("pl", Lam "n" (Lam "m" (Case n [
        (Pat "Zero" [], m),
        (Pat "Succ" ["x"], App (App (Var "pl") x) (App suc m))
      ])))] (App (App (Var "pl") three) two),
      five),
    (Let [("x", Con "Zero" [])] (App (Con "Succ" []) (Var "x")),
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
  let go a e = testCase (a ++ " ~~> " ++ e) $ evalp a @?= evalp e in
  [
  go "let x=(let y=Succ in y 0) in y" "y",
  go "let x=0 in Succ x" "1",
  go "let x=True in [x]" "[True]",
  go "let id={a->a} in id [1,2,3,4,5]" "[1,2,3,4,5]",
  go "let two={a->{b->a}} in two True False" "True",
  go "{a->{b->a}}" "{a->{b->a}}",
  go "{b->{a->b} A} B" "B",
  go "{a->{x->x} A} B" "A",
  go "{x->x} {a->{b->a}}" "{a->{b->a}}",
  go "{f->{x->f x}}" "{f->{x->f x}}",
  go "let a={f->{x->f x}} in a" "{f->{x->f x}}",
  go "{f->{x->f x}} {b->T} F" "T",
  go "let a={f->{x->f x}} in a {b->T} F" "T",
  go "let a={f->{x->f x}} in a {b->T}" "{x->{b->T} x}",
  go "let a={f->{x->f x}} in a {b->Succ b}" "{x->{b->Succ b} x}",
  go "{a->Succ (Succ a)} 1" "3",
  go "let sumtwo={a->Succ (Succ a)} in sumtwo 1" "3",
  go "let a={f->{x->f x}} in a {n->Succ n} 0" "1",
  go "let two={a->{b->a}} in {x->x} (two A) " "{b->A}",
  go "let two={a->{b->a}} in let id={c->c} in id two" "{a->{b->a}}",
  go "let fst={a->{b->a}} in let id={b->b} in id fst" "{a->{b->a}}",
  go "let fst={a->{b->a}} in let id={a->a} in id fst" "{a->{b->a}}",
  go "let f={a->{b->a}} in let p=f T in let a={f->{x->f x}} in a p F" "T",
  go "let id={a->a} in let a={f->{x->f x}} in a id [1,2,3,4]" "[1,2,3,4]",
  go "let cp={a->case a of Zero->0;Succ b->Succ (cp b);} in cp 4" "4",
  go "let fst={t->case t of Tup x y->x;} in fst (Tup 1 2)" "1",
  go "let x=A B in Tup x x" "Tup (A B) (A B)",
  go "(let x=F in x) (let x=X in x)" "F X",
  go "{n->case n of S n -> n;} (S Z)" "Z",
  go "{n->case n of S n -> n;} (S n)" "n"
  ]

evalWithPreludeTest :: TestTree
evalWithPreludeTest = testGroup "evalPreludeTest" $
  map (\(a, e) ->
    testCase (a ++ " ~~> " ++ e) $ (evalp . (++) prelude) a @?= evalp e)
  [
    ("x", "x"),
    ("copyN 5", "5"),
    ("{x->x} (copyN 5)", "5"),
    ("len [1,2,3,4,5,6,7]", "7"),
    ("plus 2 3", "5"),
    ("mult 4 5", "20"),
    ("append [1,2,3,4] [5,6,7]", "[1,2,3,4,5,6,7]"),
    ("append [] [One,Two,Three]", "[One,Two,Three]"),
    ("append [One,Two,Three] []", "[One,Two,Three]"),
    ("append (append [1,2] [3]) [4,5,6]", "[1,2,3,4,5,6]"),
    ("reverse' [A,B,C,D]", "[D,C,B,A]"),
    ("reverse' []", "[]"),
    ("reverse' [One]", "[One]"),
    ("reverse []", "[]"),
    ("reverse [A]", "[A]"),
    ("reverse [A,B,C,D,E,F]", "[F,E,D,C,B,A]"),
    ("map id [A,B,C,D,E]", "[A,B,C,D,E]"),
    ("reverse' (map reverse' [[A,B,C], [D,E], [F]])", "[[F],[E,D],[C,B,A]]"),
    ("let plus2={n -> plus 2 n} in plus2 1", "3"),
    ("let multten=mult 10 in multten 1", "10"),
    ("map {a->A} [1,2,3,4,5]", "[A,A,A,A,A]"),
    ("map {a->a} [1,2,3,4,5]", "[1,2,3,4,5]"),
    ("map {a->Succ (Succ a)} [1]", "[3]"),
    ("map {a->Succ (Succ a)} [1,2,3,4,5]", "[3,4,5,6,7]"),
    ("map {q->plus 10 q} [1,2,3,4,5]", "[11,12,13,14,15]"),
    ("let p10={q->plus 10 q} in map p10 [1,2,3,4,5]", "[11,12,13,14,15]"),
    ("let two={a->{b->a}} in let p10=two 10 in app p10 10", "10"),
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

evalLazyTest :: TestTree
evalLazyTest = testGroup "eval lazy with prelude" $
  let tc a e = testCase (a ++ " ~~> " ++ e) in
  let go a e = tc a e $ (evalp . (++) prelude) a @?= evalp e in
  [
    go "head infA" "A",
    go "head (inf 1)" "1",
    go "head (tail (tail (tail (inf 1))))" "4"
  ]

evalNameCaptureTest = testGroup "eval name capture: eval . parseExpr" $
  let go a e = testCase (a ++ " ~~> " ++ e) $ evalp a @?= evalp e in
  [
    go "let x=(let y=A in y 0) in x y" "A 0 y",
    go "let x=(let y=A in let z=B in C y z) in x y z" "C A B y z",
    go "(let x=A in C x) x" "C A x",
    go "(let y=A in let z=B in C y z) y z" "C A B y z",
    go "let a=b in let b=A in b" "A",
    go "let a=b in let b=A in a" "b",
    go "(let b=A in b) (let a=b in a)" "A b",
    go "case Succ n of Succ n'->A n';" "A n",
    go "case Succ n of Succ n->A n;" "A n"
  ]

main :: IO ()
main = defaultMain $ testGroup "Eval::eval/whnf"
  [whnfTest, evalTest, evalWithParseTest, evalWithPreludeTest,
  evalLazyTest, evalNameCaptureTest, whnfcTest, evalcTest]
