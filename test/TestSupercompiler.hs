
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, assertBool)

import Expr (Expr(Var, Con, Lam, Let, App, Case),
  app, nil, cons, bool, nat, list)
import Parser (parseExpr)
import Eval (eval)
import Supercompiler (supercompile)

supercompileWithPrelude :: String -> (Expr, Expr)
supercompileWithPrelude exprText = (expr, sexpr)
  where
    expr = parseExpr $ prelude ++ exprText
    sexpr = supercompile expr
    prelude =
      "let id={x -> x} in \
      \let app={p->{q->p q}} in \
      \let c={p->{q->{x->p (q x)}}} in \
      \let inc={n->Succ n} in \
      \let copyn={n->case n of \
      \  Zero -> 0;\
      \  Succ n' -> Succ (copyn n');} in \
      \let eqn={n->{m->case n of \
      \  Zero -> case m of \
      \    Zero -> True; \
      \    Succ m' -> False;; \
      \  Succ n' -> case m of \
      \    Zero -> False; \
      \    Succ m' -> eqn n' m';; \
      \  }} in \
      \let leqn = {n->{m->case n of \
      \  Zero -> True; \
      \  Succ n' -> case m of \
      \    Zero -> False; \
      \    Succ m' -> leqn n' m';;}} in \
      \let plus={n->{m->case n of \
      \    Zero -> m; \
      \    Succ n' -> Succ (plus n' m); \
      \  }} in \
      \let mult={n->{m->case n of \
      \  Zero -> 0;\
      \  Succ nn -> plus (mult nn m) m;}} in \
      \let map={f->{xs->case xs of \
      \  Nil->Nil;\
      \  Cons y ys->Cons (f y)(map f ys);}}in \
      \let len={xs->case xs of \
      \  Nil->0;\
      \  Cons y ys->Succ (len ys);} in \
      \let append={xs->{ys->case xs of \
      \  Nil->ys;\
      \  Cons z zs->Cons z (append zs ys);}} in \
      \let reverse={rs-> case rs of \
      \  Nil->Nil;\
      \  Cons s ss->append (reverse ss) [s];} in \
      \let reverseAccum'={xs->{as->case xs of \
      \  Nil->as;\
      \  Cons y ys->reverseAccum' ys (Cons y as);}} in \
      \let reverseAccum={rs->reverseAccum' rs []} in \
      \let insertSorted = {x->{xs-> case xs of \
      \  Nil -> Cons x Nil; \
      \  Cons y ys -> case leqn x y of \
      \    True -> Cons x (Cons y ys); \
      \    False -> Cons y (insertSorted x ys);;}} in "

main :: IO ()
main = defaultMain $ testGroup "Supercompile Test" [
  testGroup "Eval" [
    go "map inc zs" $
      Let "zs" . list nat,
    go "map inc zs" $
      Let "zs" . list bool,
    go "map inc zs" $
      Let "zs" . list (list nat),
    go "map inc" $
      flip App . list nat,
    go "map inc (map inc zs)" $
      Let "zs" . list nat,
    go "map {n->Succ n} (map {n->Succ(Succ n)} zs)" $
      \zs -> Let "zs" (list nat zs),
    go "map h (map g zs)" $
      \zs ->
        Let "h" (parseExpr
          "{n->Succ n}") .
        Let "g" (parseExpr
          "{n->case n of Zero->Zero;Succ n'->Succ(Succ (g n'));}") .
        Let "zs" (list nat zs),
    go "let mimi={zs->map inc (map inc zs)} in mimi" $
      flip App . list nat,
    go "c (map inc) (map inc)" $
      flip App . list nat,
    go "append as bs" $
      \(as, bs) -> Let "as" (list nat as) . Let "bs" (list nat bs),
    go "append (append as bs) cs" $
      \(as, bs, cs) ->
        Let "as" (list nat as) .
          Let "bs" (list nat bs) .
            Let "cs" (list nat cs),
    go "reverse zs" $
      Let "zs" . list nat,
    go "reverseAccum zs" $
      Let "zs" . list nat
  ],
  testGroup "Predicates" [
    goPred "eqn x x" $
      Let "x" . nat,
    goPred "eqn (plus Zero x) x" $
      Let "x" . nat,
    goPred "eqn x (plus Zero x)" $
      Let "x" . nat,
    goPred "eqn (len (map id zs)) (len zs)" $
      \zs -> Let "zs" $ list nat zs,
    goPred "eqn (len (map f zs)) (len zs)" $
      \zs -> Let "f" (parseExpr "{n->Succ n}") . Let "zs" (list nat zs),
    goPred "eqn (len (append as bs)) (plus (len as) (len bs))" $
      \(as, bs) -> Let "as" (list nat as) . Let "bs" (list nat bs),
    goPred "leqn 0 x" $
      Let "x" . nat,
    goPred "eqn (len (insertSorted z zs)) (plus 1 (len zs))" $
      \(z, zs) -> Let "x" (nat z) . Let "zs" (list nat zs)
    ],
  testGroup "Invertible Functions" [
    go "eqn x 0" $
      Let "x" . nat,
    go "eqn x 5" $
      Let "x" . nat
    ]
  ]
  where
    go e fexpr = let (expr, sexpr) = supercompileWithPrelude e in
      testProperty e $ (\cexpr ->
        eval (cexpr sexpr) == eval (cexpr expr)) . fexpr
    goPred exprText fexpr =
      let (expr, sexpr) = supercompileWithPrelude exprText in
      testGroup exprText [
        testProperty "(Eval)" $ (\cexpr ->
          eval (cexpr sexpr) == eval (cexpr expr)) . fexpr,
        testCase "(True)" $ assertBool exprText (onlyTrue sexpr)
      ]
    onlyTrue expr = case expr of
      Var _ -> True
      Con tag args -> tag == "True" && null args
      Lam _ lamexpr -> onlyTrue lamexpr
      Let _ valexpr inexpr -> onlyTrue valexpr && onlyTrue inexpr
      App funexpr valexpr -> onlyTrue funexpr && onlyTrue valexpr
      Case scexpr pats -> onlyTrue scexpr && all (onlyTrue . snd) pats
