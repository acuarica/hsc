-- {-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Expr (Expr(App, Let), app, nil, cons, nat)
import Parser (parseExpr)
import Eval (eval)
import Supercompiler (supercompile)

ls :: [Int] -> Expr
ls [] = nil
ls (x:xs) = app cons [nat x, ls xs]

supercompileWithEvalTest :: TestTree
supercompileWithEvalTest = testGroup "Supercompile & Eval" [
  go "map inc zs" $
    Let "zs" . ls,
  go "map inc" $
    flip App . ls,
  go "map inc (map inc zs)" $
    Let "zs" . ls,
  go "map {n->Succ n} (map {n->Succ(Succ n)} zs)" $
    \zs -> Let "zs" (ls zs),
  go "map h (map g zs)" $
    \zs ->
      Let "h" (parseExpr
        "{n->Succ n}") .
      Let "g" (parseExpr
        "{n->case n of Zero->Zero;Succ n'->Succ(Succ (g n'));}") .
      Let "zs" (ls zs),
  go "let mimi={zs->map inc (map inc zs)} in mimi" $
    flip App . ls,
  go "c (map inc) (map inc)" $
    flip App . ls,
  go "append as bs" $
    \(as, bs) -> Let "as" (ls as) . Let "bs" (ls bs),
  go "append (append as bs) cs" $
    \(as, bs, cs) -> Let "as" (ls as).Let "bs" (ls bs).Let "cs" (ls cs),
  go "eqn x x" $
    Let "x" . nat,
  go "eqn (plus Zero x) x" $
    Let "x" . nat,
  go "eqn (len (map id zs)) (len zs)" $
    \zs -> Let "zs" $ ls zs,
  go "eqn (len (map f zs)) (len zs)" $
    \zs -> Let "f" (parseExpr "{n->Succ n}") . Let "zs" (ls zs),
  go "eqn (len (append as bs)) (plus (len as) (len bs))" $
    \(as, bs) -> Let "as" (ls as) . Let "bs" (ls bs),
  go "leqn 0 x" $
    Let "x" . nat,
  go "eqn (len (insertSorted z zs)) (plus 1 (len zs))" $
    \(z, zs) -> Let "x" (nat z) . Let "zs" (ls zs),
  go "eqn x 0" $
    Let "x" . nat,
  go "eqn x 5" $
    Let "x" . nat
  ]
  where
    go e fexpr =
      let expr = parseExpr $ prelude ++ e in
      let sexpr = supercompile expr in
      testProperty e $ (\cexpr ->
        eval (cexpr sexpr) == eval (cexpr expr)) . fexpr
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
main = defaultMain supercompileWithEvalTest
