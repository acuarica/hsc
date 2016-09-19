{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Test.Tasty --(TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.SmallCheck

import Control.Arrow (second)

import Expr --(Expr(..), appVars)
import Parser (parseExpr)
import Eval (Env, newConf, emptyEnv, eval)
import Supercompiler (supercompile)

ls :: [Int] -> Expr
ls [] = nil
ls (x:xs) = app cons [f x, ls xs]
  where f n = if n > 0 then App suc (f (n-1)) else zero

testSupercompile = testGroup "Supercompiler" [
  go "map inc zs" $
    Let "zs" . ls,
  go "map inc" $
    flip App . ls,
  go "map inc (map inc zs)" $
    Let "zs" . ls,
  go "map h (map g zs)" $
    \zs -> Let "h" (Var "inc") . Let "g" (Var "inc") . Let "zs" (ls zs),
  go "let mimi={zs->map inc (map inc zs)} in mimi" $
    flip App . ls,
  go "c (map inc) (map inc)" $
    flip App . ls,
  go "append as bs" $
    \(as, bs) -> Let "as" (ls as) . Let "bs" (ls bs),
  go "append (append as bs) cs" $
    \(as, bs, cs) -> Let "as" (ls as) . Let "bs" (ls bs) . Let "cs" (ls cs)
  ]
  --revzs = "rev vs"
  --revAccum = "let reverse={rs->revAccum rs []} in reverse zs"
  where
    go e fexpr =
      let expr = parseExpr $ prelude ++ e in
      let sexpr = supercompile expr in
      testProperty (e ++ show sexpr) $ (\cexpr ->
        eval (cexpr sexpr) == eval (cexpr expr)) . fexpr
    prelude =
      "let id={x->x} in \
      \let app={p->{q->p q}} in \
      \let c={p->{q->{x->p (q x)}}} in \
      \let inc={n->Succ n} in \
      \let cp={a->case a of Zero->0;Succ aa->Succ (cp aa);} in \
      \let append={xs->{ys->case xs of Nil->ys;Cons z zs->Cons z (append zs ys);}} in \
      \let rev={rs-> case rs of Nil->Nil;Cons s ss->append (rev ss) [s];} in \
      \let revA={xs->{as->case xs of Nil->as;Cons y ys->revA ys (Cons y as);}}in \
      \let map={f->{xs->case xs of Nil->Nil;Cons y ys->Cons (f y)(map f ys);}}in \
      \let plus={n->{m->case n of Zero->m; Succ nn->plus nn (Succ m);}} in \
      \let mult={n->{m->case n of Zero->0; Succ nn->plus (mult nn m) m;}} in \
      \let len={xs->case xs of Nil->0; Cons y ys->Succ (len ys);} in "

main :: IO ()
main = defaultMain testSupercompile
